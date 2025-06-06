module NixTree.StorePath
  ( StoreName (..),
    storeNameToPath,
    storeNameToText,
    storeNameToShortText,
    storeNameToSplitShortText,
    storeNameToShortTextWithDisambiguation,
    NixPathSignature (..),
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
    mkStoreName,
    storeEnvToDot,
  )
where

import Data.Aeson (FromJSON (..), Value (..), decode, eitherDecode, (.!=), (.:), (.:?))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Dot
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

getStoreDir :: Maybe FilePath -> IO NixStore
getStoreDir seoNixStore = do
  let prog = "nix-instantiate"
      args =
        ["--eval", "--expr", "(builtins.storeDir)", "--json"]
          ++ ( case seoNixStore of
                 Nothing -> []
                 Just url -> ["--option", "store", url]
             )
  out <-
    readProcessStdout_ (proc prog args)
      <&> fmap mkNixStore
      . decode @FilePath
  case out of
    Nothing -> fail $ "Error interpreting output of: " ++ show (prog, args)
    Just p -> return p

--------------------------------------------------------------------------------

data StoreName = StoreName NixStore Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

mkStoreName :: NixStore -> FilePath -> Maybe StoreName
mkStoreName ns path = do
  let ps = unNixStore ns
  guard $ ps `isPrefixOf` path
  let ds = splitDirectories (drop (length ps) path)
  sn <- listToMaybe ds
  return $ StoreName ns (toText sn)

storeNameToText :: StoreName -> Text
storeNameToText (StoreName _ n) = n

storeNameToPath :: StoreName -> FilePath
storeNameToPath (StoreName ns sn) = unNixStore ns </> toString sn

storeNameToShortText :: StoreName -> Text
storeNameToShortText = snd . storeNameToSplitShortText

storeNameToShortTextWithDisambiguation :: Int -> StoreName -> Text
storeNameToShortTextWithDisambiguation 0 sn = storeNameToShortText sn
storeNameToShortTextWithDisambiguation n (StoreName ns sn) =
  let (f, s) = storeNameToSplitShortText (StoreName ns sn)
   in T.take n f <> "...-" <> s

storeNameToSplitShortText :: StoreName -> (Text, Text)
storeNameToSplitShortText txt =
  case T.span (/= '-') $ storeNameToText txt of
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

data StorePath ref payload = StorePath
  { spName :: StoreName,
    spSize :: Int,
    spRefs :: [ref],
    spPayload :: payload,
    spSignatures :: [NixPathSignature]
  }
  deriving (Show, Eq, Ord, Functor, Generic)

instance (NFData a, NFData b) => NFData (StorePath a b)

newtype Installable = Installable {installableToText :: Text}

--------------------------------------------------------------------------------

data PathInfoOptions = PathInfoOptions
  { pioIsRecursive :: Bool,
    pioIsDerivation :: Bool,
    pioIsImpure :: Bool,
    pioStoreURL :: Maybe FilePath,
    pioFile :: Maybe FilePath
  }

getPathInfo :: NixStore -> NixVersion -> PathInfoOptions -> NonEmpty Installable -> IO (NonEmpty (StorePath StoreName ()))
getPathInfo nixStore nixVersion options names = do
  infos <-
    eitherDecode @NixPathOutput
      <$> readProcessStdout_
        ( proc
            "nix"
            ( ["path-info", "--json"]
                ++ ["--impure" | options & pioIsImpure]
                ++ ["--recursive" | options & pioIsRecursive]
                ++ ["--derivation" | (options & pioIsDerivation) && nixVersion >= Nix2_4]
                ++ ( case options & pioStoreURL of
                       Nothing -> []
                       Just url -> ["--store", url]
                   )
                ++ ( case options & pioFile of
                       Nothing -> []
                       Just file -> ["--file", file]
                   )
                ++ (if nixVersion >= Nix2_4 then ["--extra-experimental-features", "nix-command flakes"] else [])
                ++ map (toString . installableToText) (toList names)
            )
        )
      >>= either (\e -> fail $ "Failed parsing nix path-info output: " ++ show e) return
      >>= mapM assertValidInfo . npoResults
      >>= maybe (fail "invariant violation: getPathInfo returned []") return . nonEmpty

  mapM infoToStorePath infos
  where
    infoToStorePath NixPathInfo {npiPath, npiNarSize, npiReferences, npiSignatures} = do
      name <- mkStoreNameIO npiPath
      refs <- filter (/= name) <$> mapM mkStoreNameIO npiReferences
      return $
        StorePath
          { spName = name,
            spRefs = refs,
            spSize = npiNarSize,
            spSignatures = npiSignatures,
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

data StoreEnv payload = StoreEnv
  { sePaths :: HashMap StoreName (StorePath StoreName payload),
    seRoots :: NonEmpty StoreName
  }
  deriving (Functor, Generic, NFData)

data StoreEnvOptions = StoreEnvOptions
  { seoIsDerivation :: Bool,
    seoIsImpure :: Bool,
    seoStoreURL :: Maybe String,
    seoFile :: Maybe String
  }

withStoreEnv ::
  forall m a.
  (MonadIO m) =>
  StoreEnvOptions ->
  NonEmpty Installable ->
  (StoreEnv () -> m a) ->
  m a
withStoreEnv StoreEnvOptions {seoIsDerivation, seoIsImpure, seoStoreURL, seoFile} names cb = do
  nixStore <- liftIO $ getStoreDir seoStoreURL

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
        (PathInfoOptions {pioIsDerivation = seoIsDerivation, pioIsRecursive = False, pioIsImpure = seoIsImpure, pioStoreURL = seoStoreURL, pioFile = seoFile})
        names

  paths <-
    liftIO $
      getPathInfo
        nixStore
        nixVersion
        (PathInfoOptions {pioIsDerivation = seoIsDerivation, pioIsRecursive = True, pioIsImpure = seoIsImpure, pioStoreURL = seoStoreURL, pioFile = seoFile})
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

seLookup :: StoreEnv a -> StoreName -> StorePath StoreName a
seLookup StoreEnv {sePaths} name =
  fromMaybe
    (error $ "invariant violation, StoreName not found: " <> show name)
    (HM.lookup name sePaths)

seAll :: StoreEnv a -> NonEmpty (StorePath StoreName a)
seAll StoreEnv {sePaths} = case HM.elems sePaths of
  [] -> error "invariant violation: no paths"
  (x : xs) -> x :| xs

seGetRoots :: StoreEnv a -> NonEmpty (StorePath StoreName a)
seGetRoots env@StoreEnv {seRoots} =
  fmap (seLookup env) seRoots

seFetchRefs ::
  StoreEnv a ->
  (StoreName -> Bool) ->
  NonEmpty StoreName ->
  [StorePath StoreName a]
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
  forall a b.
  (StorePath (StorePath StoreName b) a -> b) ->
  StoreEnv a ->
  StoreEnv b
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
      StoreName ->
      State
        ( HashMap StoreName (StorePath StoreName a),
          HashMap StoreName (StorePath StoreName b)
        )
        (StorePath StoreName b)
    go name = do
      processed <- gets snd
      case name `HM.lookup` processed of
        Just sp -> return sp
        Nothing -> do
          sp@StorePath {spName, spRefs} <- unsafeLookup name <$> gets fst
          refs <- mapM go spRefs
          let new = sp {spPayload = f sp {spRefs = refs}}
          modify $ bimap (HM.delete spName) (HM.insert spName new)
          return new

--------------------------------------------------------------------------------

storeEnvToDot :: StoreEnv a -> Text
storeEnvToDot env =
  seBottomUp go env
    & seGetRoots
    & toList
    & map spPayload
    & mconcat
    & render
  where
    go sp =
      fromList [Set.singleton (spName sp, spName ref) <> spPayload ref | ref <- spRefs sp]
        & mconcat

    render :: Set (StoreName, StoreName) -> Text
    render edges =
      Dot.DotGraph
        Dot.Strict
        Dot.Directed
        Nothing
        [ Dot.StatementEdge
            ( Dot.EdgeStatement
                (Dot.ListTwo (Dot.EdgeNode (mkNodeId from)) (Dot.EdgeNode (mkNodeId to)) [])
                []
            )
          | (from, to) <- toList edges
        ]
        & Dot.encode

    mkNodeId :: StoreName -> Dot.NodeId
    mkNodeId = fromString . toString . storeNameToShortText

--------------------------------------------------------------------------------

data NixPathInfo = NixPathInfo
  { npiPath :: FilePath,
    npiNarSize :: Int,
    npiReferences :: [FilePath],
    npiSignatures :: [NixPathSignature]
  }

data NixPathSignature = NixPathSignature
  { npsKeyName :: Text,
    npsSignature :: Text
  }
  deriving (Show, Eq, Ord, NFData, Generic)

instance FromJSON NixPathSignature where
  parseJSON (String t) =
    case T.splitOn ":" t of
      [key, sig]
        | not (T.null key) && not (T.null sig) ->
            return $ NixPathSignature key sig
      _ -> fail "Expecting a string in the form of 'key:sig'."
  parseJSON _ = fail "Expecting a string."

data NixPathInfoResult
  = NixPathInfoValid NixPathInfo
  | NixPathInfoInvalid FilePath

parse2_18 :: Value -> Parser NixPathInfoResult
parse2_18 (Object obj) =
  ( NixPathInfoValid
      <$> ( NixPathInfo
              <$> obj .: "path"
              <*> obj .: "narSize"
              <*> obj .: "references"
              <*> (obj .:? "signatures" .!= [])
          )
  )
    <|> ( do
            path <- obj .: "path"
            valid <- obj .: "valid"
            guard (not valid)
            return $ NixPathInfoInvalid path
        )
parse2_18 _ = fail "Expecting an object."

parse2_19 :: (FilePath, Value) -> Parser NixPathInfoResult
parse2_19 (path, Object obj) =
  NixPathInfoValid
    <$> ( NixPathInfo
            path
            <$> obj .: "narSize"
            <*> obj .: "references"
            <*> (obj .:? "signatures" .!= [])
        )
parse2_19 (path, Null) = return $ NixPathInfoInvalid path
parse2_19 (_, _) = fail "Expecting an object or null"

newtype NixPathOutput = NixPathOutput
  { npoResults :: [NixPathInfoResult]
  }

instance FromJSON NixPathOutput where
  parseJSON (Array a) = NixPathOutput <$> mapM parse2_18 (toList a)
  parseJSON (Object o) = NixPathOutput <$> mapM (parse2_19 . first K.toString) (KM.toList o)
  parseJSON _ = fail "Expecting an array (nix<=2.18) or an object with mapping from path to info (nix>=2.19)."
