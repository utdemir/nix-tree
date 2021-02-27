module StorePath
  ( StoreName (..),
    storeNameToPath,
    storeNameToText,
    storeNameToShortText,
    storeNameToSplitShortText,
    StorePath (..),
    StoreEnv (..),
    withStoreEnv,
    seLookup,
    seAll,
    seGetRoots,
    seBottomUp,
    seFetchRefs,
  )
where

import Control.Monad (fail)
import Data.Aeson (FromJSON (..), Value (..), decode, (.:))
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import System.FilePath.Posix (splitDirectories)
import System.Process.Typed (proc, readProcessStdout_)

--------------------------------------------------------------------------------

newtype StoreName s = StoreName Text
  deriving newtype (Show, Eq, Ord, Hashable, NFData)

mkStoreName :: FilePath -> Maybe (StoreName a)
mkStoreName path =
  case splitDirectories path of
    "/" : "nix" : "store" : name : _ -> Just . StoreName $ toS name
    _ -> Nothing

storeNameToText :: StoreName a -> Text
storeNameToText (StoreName n) = n

storeNameToPath :: StoreName a -> FilePath
storeNameToPath (StoreName sn) = "/nix/store/" <> toS sn

storeNameToShortText :: StoreName a -> Text
storeNameToShortText = snd . storeNameToSplitShortText

storeNameToSplitShortText :: StoreName a -> (Text, Text)
storeNameToSplitShortText txt =
  case T.span (/= '-') . T.pack $ storeNameToPath txt of
    (f, s) | Just (c, s'') <- T.uncons s -> (T.snoc f c, s'')
    e -> e

--------------------------------------------------------------------------------

data StorePath s ref payload = StorePath
  { spName :: StoreName s,
    spSize :: Int,
    spRefs :: [ref],
    spPayload :: payload
  }
  deriving (Show, Eq, Ord, Functor, Generic)

instance (NFData a, NFData b) => NFData (StorePath s a b)

mkStorePaths :: NonEmpty (StoreName s) -> IO [StorePath s (StoreName s) ()]
mkStorePaths names = do
  -- See: https://github.com/utdemir/nix-tree/issues/12
  --
  -- > In Nix < 2.4, when you pass a .drv to path-info, it returns information about the store
  -- > derivation. However, when you do the same in 2.4, it "resolves" it and works on
  -- > the output of given derivation; to actually work on the derivation you need to pass
  -- > --derivation.
  isAtLeastNix24 <- (>= Just "2.4") <$> getNixVersion
  let (derivations, outputs) =
        partition
          (\i -> ".drv" `T.isSuffixOf` storeNameToText i)
          (NE.toList names)
  (++)
    <$> maybe (return []) (getPathInfo False) (NE.nonEmpty outputs)
    <*> maybe (return []) (getPathInfo (True && isAtLeastNix24)) (NE.nonEmpty derivations)
  where
    getNixVersion :: IO (Maybe Text)
    getNixVersion = do
      out <- decodeUtf8 . BL.toStrict <$> readProcessStdout_ (proc "nix" ["--version"])
      return . lastMay $ T.splitOn " " out

getPathInfo :: Bool -> NonEmpty (StoreName s) -> IO [StorePath s (StoreName s) ()]
getPathInfo isDrv names = do
  infos <-
    decode @[NixPathInfoResult]
      <$> readProcessStdout_
        ( proc
            "nix"
            ( ["path-info", "--recursive", "--json"]
                ++ (if isDrv then ["--derivation"] else [])
                ++ map storeNameToPath (toList names)
            )
        )
      >>= maybe (fail "Failed parsing nix path-info output.") return
      >>= mapM assertValidInfo
  mapM infoToStorePath infos
  where
    infoToStorePath NixPathInfo {npiPath, npiNarSize, npiReferences} = do
      name <- mkStoreNameIO npiPath
      refs <- filter (/= name) <$> mapM mkStoreNameIO npiReferences
      return $
        StorePath
          { spName = name,
            spRefs = refs,
            spSize = npiNarSize,
            spPayload = ()
          }
    mkStoreNameIO p =
      maybe
        (fail $ "Failed parsing Nix store path: " ++ show p)
        return
        (mkStoreName p)

    assertValidInfo (NixPathInfoValid pathinfo) = return pathinfo
    assertValidInfo (NixPathInfoInvalid path) =
      fail $ "Invalid path: " ++ path ++ ". Inconsistent /nix/store or ongoing GC."

--------------------------------------------------------------------------------

data StoreEnv s payload = StoreEnv
  { sePaths :: HashMap (StoreName s) (StorePath s (StoreName s) payload),
    seRoots :: NonEmpty (StoreName s)
  }
  deriving (Functor, Generic, NFData)

withStoreEnv ::
  forall m a.
  MonadIO m =>
  NonEmpty FilePath ->
  (forall s. StoreEnv s () -> m a) ->
  m (Either [FilePath] a)
withStoreEnv fnames cb = do
  let names' =
        fnames
          & toList
          & map (\f -> maybe (Left f) Right (mkStoreName f))
          & partitionEithers

  case names' of
    (errs@(_ : _), _) -> return (Left errs)
    ([], xs) -> case nonEmpty xs of
      Nothing -> panic "invariant violation"
      Just names -> do
        paths <- liftIO $ mkStorePaths names
        let env =
              StoreEnv
                ( paths
                    & map (\p@StorePath {spName} -> (spName, p))
                    & HM.fromList
                )
                names
        Right <$> cb env

seLookup :: StoreEnv s a -> StoreName s -> StorePath s (StoreName s) a
seLookup StoreEnv {sePaths} name =
  fromMaybe
    (panic $ "invariant violation, StoreName not found: " <> show name)
    (HM.lookup name sePaths)

seAll :: StoreEnv s a -> NonEmpty (StorePath s (StoreName s) a)
seAll StoreEnv {sePaths} = case HM.elems sePaths of
  [] -> panic "invariant violation: no paths"
  (x : xs) -> x :| xs

seGetRoots :: StoreEnv s a -> NonEmpty (StorePath s (StoreName s) a)
seGetRoots env@StoreEnv {seRoots} =
  map (seLookup env) seRoots

seFetchRefs ::
  StoreEnv s a ->
  (StoreName s -> Bool) ->
  NonEmpty (StoreName s) ->
  [StorePath s (StoreName s) a]
seFetchRefs env predicate =
  fst
    . foldl'
      (\(acc, visited) name -> go acc visited name)
      ([], HS.empty)
  where
    go acc visited name
      | HS.member name visited = (acc, visited)
      | not (predicate name) = (acc, visited)
      | otherwise =
        let sp@StorePath {spRefs} = seLookup env name
         in foldl'
              (\(acc', visited') name' -> go acc' visited' name')
              (sp : acc, HS.insert name visited)
              spRefs

seBottomUp ::
  forall s a b.
  (StorePath s (StorePath s (StoreName s) b) a -> b) ->
  StoreEnv s a ->
  StoreEnv s b
seBottomUp f StoreEnv {sePaths, seRoots} =
  StoreEnv
    { sePaths = snd $ execState (mapM_ go seRoots) (sePaths, HM.empty),
      seRoots
    }
  where
    unsafeLookup k m =
      fromMaybe
        (panic $ "invariant violation: name doesn't exists: " <> show k)
        (HM.lookup k m)
    go ::
      StoreName s ->
      State
        ( HashMap (StoreName s) (StorePath s (StoreName s) a),
          HashMap (StoreName s) (StorePath s (StoreName s) b)
        )
        (StorePath s (StoreName s) b)
    go name = do
      processed <- gets snd
      case name `HM.lookup` processed of
        Just sp -> return sp
        Nothing -> do
          sp@StorePath {spName, spRefs} <- unsafeLookup name <$> gets fst
          refs <- mapM go spRefs
          let new = sp {spPayload = f sp {spRefs = refs}}
          modify
            ( \(as, bs) ->
                ( HM.delete spName as,
                  HM.insert spName new bs
                )
            )
          return new

--------------------------------------------------------------------------------

data NixPathInfo = NixPathInfo
  { npiPath :: FilePath,
    npiNarSize :: Int,
    npiReferences :: [FilePath]
  }

data NixPathInfoResult
  = NixPathInfoValid NixPathInfo
  | NixPathInfoInvalid FilePath

instance FromJSON NixPathInfoResult where
  parseJSON (Object obj) =
    ( NixPathInfoValid
        <$> ( NixPathInfo
                <$> obj .: "path"
                <*> obj .: "narSize"
                <*> obj .: "references"
            )
    )
      <|> ( do
              path <- obj .: "path"
              valid <- obj .: "valid"
              guard (not valid)
              return $ NixPathInfoInvalid path
          )
  parseJSON _ = fail "Expecting an object."
