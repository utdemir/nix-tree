module NixTree.StorePath
  ( StoreName (..),
    storeNameToPath,
    storeNameToText,
    storeNameToShortText,
    storeNameToSplitShortText,
    StorePath (..),
    Installable (..),
    StoreEnv (..),
    StoreEnvOptions (..),
    withStoreEnv,
    seLookup,
    seAll,
    seGetRoots,
    seBottomUp,
    seFetchRefs,
    getNixStore,
    mkStoreName,
  )
where

import Data.Aeson (FromJSON (..), Value (..), decode, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import System.FilePath.Posix (addTrailingPathSeparator, splitDirectories, (</>))
import System.IO (hPutStrLn)
import System.Process.Typed (proc, readProcessStdout_)

-- Technically these both are filepaths. However, most people use the default "/nix/store",
-- hence special casing it speeds things up.
data NixStore
  = NixStore
  | NixStoreCustom FilePath
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

mkNixStore :: FilePath -> NixStore
mkNixStore i' =
  let i = addTrailingPathSeparator i'
   in if i == "/nix/store/" then NixStore else NixStoreCustom i

unNixStore :: NixStore -> FilePath
unNixStore NixStore = "/nix/store/"
unNixStore (NixStoreCustom fp) = fp

getNixStore :: IO NixStore
getNixStore = do
  let prog = "nix-instantiate"
      args = ["--eval", "--expr", "(builtins.storeDir)", "--json"]
  out <-
    readProcessStdout_ (proc prog args)
      <&> fmap mkNixStore . decode @FilePath
  case out of
    Nothing -> fail $ "Error interpreting output of: " ++ show (prog, args)
    Just p -> return p

--------------------------------------------------------------------------------

data StoreName s = StoreName NixStore Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

mkStoreName :: NixStore -> FilePath -> Maybe (StoreName a)
mkStoreName ns path = do
  let ps = unNixStore ns
  guard $ ps `isPrefixOf` path
  let ds = splitDirectories (drop (length ps) path)
  sn <- listToMaybe ds
  return $ StoreName ns (toText sn)

storeNameToText :: StoreName a -> Text
storeNameToText (StoreName _ n) = n

storeNameToPath :: StoreName a -> FilePath
storeNameToPath (StoreName ns sn) = unNixStore ns </> toString sn

storeNameToShortText :: StoreName a -> Text
storeNameToShortText = snd . storeNameToSplitShortText

storeNameToSplitShortText :: StoreName a -> (Text, Text)
storeNameToSplitShortText txt =
  case T.span (/= '-') . T.pack $ storeNameToPath txt of
    (f, s) | Just (c, s'') <- T.uncons s -> (T.snoc f c, s'')
    e -> e

--------------------------------------------------------------------------------

data NixVersion
  = NixOlder
  | Nix2_4
  | NixNewer
  | NixUnknown
  deriving (Show, Eq, Ord)

getNixVersion :: IO NixVersion
getNixVersion = do
  out <- decodeUtf8 . BL.toStrict <$> readProcessStdout_ (proc "nix" ["--version"])

  -- Parses strings like:
  --  nix (Nix) 2.6.0pre20211217_6e6e998
  --  nix (Nix) 2.5.1
  return . fromMaybe NixUnknown $ do
    -- get the last space delimited part
    ver <-
      out
        & T.splitOn " "
        & viaNonEmpty last

    -- split by ".", take the first two, and convert them to numbers
    (major, minor) <- do
      (maT, miT) <- case T.splitOn "." ver of
        p1 : p2 : _ -> Just (p1, p2)
        _ -> Nothing
      ma <- readMaybe @Natural (toString maT)
      mi <- readMaybe @Natural (toString miT)
      return (ma, mi)

    -- map it to the sum
    return $ case compare (major, minor) (2, 4) of
      LT -> NixOlder
      EQ -> Nix2_4
      GT -> NixNewer

--------------------------------------------------------------------------------

data StorePath s ref payload = StorePath
  { spName :: StoreName s,
    spSize :: Int,
    spRefs :: [ref],
    spPayload :: payload
  }
  deriving (Show, Eq, Ord, Functor, Generic)

instance (NFData a, NFData b) => NFData (StorePath s a b)

newtype Installable = Installable {installableToText :: Text}

--------------------------------------------------------------------------------

data PathInfoOptions = PathInfoOptions
  { pioIsRecursive :: Bool,
    pioIsDerivation :: Bool,
    pioIsImpure :: Bool
  }

getPathInfo :: NixStore -> NixVersion -> PathInfoOptions -> NonEmpty Installable -> IO (NonEmpty (StorePath s (StoreName s) ()))
getPathInfo nixStore nixVersion options names = do
  infos <-
    decode @[NixPathInfoResult]
      <$> readProcessStdout_
        ( proc
            "nix"
            ( ["path-info", "--json"]
                ++ (if options & pioIsImpure then ["--impure"] else [])
                ++ (if options & pioIsRecursive then ["--recursive"] else [])
                ++ (if (options & pioIsDerivation) && nixVersion >= Nix2_4 then ["--derivation"] else [])
                ++ (if nixVersion >= Nix2_4 then ["--extra-experimental-features", "nix-command flakes"] else [])
                ++ map (toString . installableToText) (toList names)
            )
        )
      >>= maybe (fail "Failed parsing nix path-info output.") return
      >>= mapM assertValidInfo
      >>= maybe (fail "invariant violation: getPathInfo returned []") return . nonEmpty

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
        (mkStoreName nixStore p)

    assertValidInfo (NixPathInfoValid pathinfo) = return pathinfo
    assertValidInfo (NixPathInfoInvalid path) =
      fail $ "Invalid path: " ++ path ++ ". Make sure that it is built, or pass '--derivation' if you want to work on the derivation."

--------------------------------------------------------------------------------

data StoreEnv s payload = StoreEnv
  { sePaths :: HashMap (StoreName s) (StorePath s (StoreName s) payload),
    seRoots :: NonEmpty (StoreName s)
  }
  deriving (Functor, Generic, NFData)

data StoreEnvOptions = StoreEnvOptions
  { seoIsDerivation :: Bool,
    seoIsImpure :: Bool
  }

withStoreEnv ::
  forall m a.
  MonadIO m =>
  StoreEnvOptions ->
  NonEmpty Installable ->
  (forall s. StoreEnv s () -> m a) ->
  m a
withStoreEnv StoreEnvOptions {seoIsDerivation, seoIsImpure} names cb = do
  nixStore <- liftIO getNixStore

  -- See: https://github.com/utdemir/nix-tree/issues/12
  nixVersion <- liftIO getNixVersion

  when (seoIsDerivation && nixVersion < Nix2_4) $
    liftIO $
      hPutStrLn stderr "Warning: --derivation flag is ignored on Nix versions older than 2.4."

  roots <-
    liftIO $
      getPathInfo
        nixStore
        nixVersion
        (PathInfoOptions {pioIsDerivation = seoIsDerivation, pioIsRecursive = False, pioIsImpure = seoIsImpure})
        names

  paths <-
    liftIO $
      getPathInfo
        nixStore
        nixVersion
        (PathInfoOptions {pioIsDerivation = seoIsDerivation, pioIsRecursive = True, pioIsImpure = seoIsImpure})
        (Installable . toText . storeNameToPath . spName <$> roots)

  let env =
        StoreEnv
          ( paths
              & toList
              & map (\p@StorePath {spName} -> (spName, p))
              & HM.fromList
          )
          (roots <&> spName)
  cb env

seLookup :: StoreEnv s a -> StoreName s -> StorePath s (StoreName s) a
seLookup StoreEnv {sePaths} name =
  fromMaybe
    (error $ "invariant violation, StoreName not found: " <> show name)
    (HM.lookup name sePaths)

seAll :: StoreEnv s a -> NonEmpty (StorePath s (StoreName s) a)
seAll StoreEnv {sePaths} = case HM.elems sePaths of
  [] -> error "invariant violation: no paths"
  (x : xs) -> x :| xs

seGetRoots :: StoreEnv s a -> NonEmpty (StorePath s (StoreName s) a)
seGetRoots env@StoreEnv {seRoots} =
  fmap (seLookup env) seRoots

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
        (error $ "invariant violation: name doesn't exists: " <> show k)
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
