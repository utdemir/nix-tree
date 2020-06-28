{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module StorePath
  ( StoreName (..),
    mkStoreName,
    storeNameToPath,
    storeNameToText,
    StorePath (..),
    StoreEnv (..),
    mkStoreEnv,
    seUnsafeLookup,
    seGetRoots,
    seBottomUp,
    seFetchRefs,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Control.Monad.Trans.State (State, execState, gets, modify)
import Data.Aeson ((.:), FromJSON (..), Value (..), decode)
import Data.Foldable (foldl')
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath.Posix (splitDirectories)
import System.Process.Typed (proc, readProcessStdout_)

--------------------------------------------------------------------------------

newtype StoreName = StoreName Text
  deriving newtype (Show, Eq, Ord, Hashable, NFData)

mkStoreName :: FilePath -> Maybe StoreName
mkStoreName path =
  case splitDirectories path of
    "/" : "nix" : "store" : name : _ -> Just . StoreName $ T.pack name
    _ -> Nothing

storeNameToText :: StoreName -> Text
storeNameToText (StoreName n) = n

storeNameToPath :: StoreName -> FilePath
storeNameToPath (StoreName sn) = "/nix/store/" ++ T.unpack sn

--------------------------------------------------------------------------------

data StorePath ref payload = StorePath
  { spName :: StoreName,
    spSize :: Int,
    spRefs :: [ref],
    spPayload :: payload
  }
  deriving (Show, Eq, Ord, Functor, Generic)

instance (NFData a, NFData b) => NFData (StorePath a b)

mkStorePaths :: NonEmpty StoreName -> IO [StorePath StoreName ()]
mkStorePaths names = do
  infos <-
    decode @[NixPathInfoResult]
      <$> readProcessStdout_
        ( proc
            "nix"
            ( ["path-info", "--recursive", "--json"]
                ++ map storeNameToPath (NE.toList names)
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
    assertValidInfo (NixPathInfoValid pi) = return pi
    assertValidInfo (NixPathInfoInvalid path) =
      fail $ "Invalid path: " ++ path ++ ". Inconsistent /nix/store or ongoing GC."

--------------------------------------------------------------------------------

data StoreEnv payload = StoreEnv
  { sePaths :: HashMap StoreName (StorePath StoreName payload),
    seRoots :: NonEmpty StoreName
  }
  deriving (Functor, Generic, NFData)

mkStoreEnv :: NonEmpty StoreName -> IO (StoreEnv ())
mkStoreEnv names = do
  paths <- mkStorePaths names
  return $
    StoreEnv
      ( paths
          & map (\p@StorePath {spName} -> (spName, p))
          & HM.fromList
      )
      names

seUnsafeLookup :: StoreEnv a -> StoreName -> StorePath StoreName a
seUnsafeLookup StoreEnv {sePaths} name =
  fromMaybe
    (error $ "invariant violation, StoreName not found: " ++ show name)
    (HM.lookup name sePaths)

seGetRoots :: StoreEnv a -> NonEmpty (StorePath StoreName a)
seGetRoots env@StoreEnv {seRoots} =
  NE.map (seUnsafeLookup env) seRoots

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
        let sp@StorePath {spRefs} = seUnsafeLookup env name
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
        (error $ "invariant violation: name doesn't exists: " ++ show k)
        (HM.lookup k m)
    go ::
      StoreName ->
      State
        ( HashMap StoreName (StorePath StoreName a),
          HashMap StoreName (StorePath StoreName b)
        )
        (StorePath StoreName b)
    go name = do
      bs <- gets snd
      case name `HM.lookup` bs of
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
