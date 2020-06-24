{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module StorePath
  ( StoreName (..),
    mkStoreName,
    storeNameToPath,
    StorePath (..),
    StoreEnv (..),
    mkStoreEnv,
    unsafeLookupStoreEnv,
    transformStoreEnv,
    fetchAllReferences,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Trans.State (State, execState, gets, modify)
import Data.Aeson ((.:), FromJSON (..), Value (..), decode)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath.Posix (splitDirectories)
import System.Process.Typed (proc, readProcessStdout_)

--------------------------------------------------------------------------------

newtype StoreName = StoreName Text
  deriving newtype (Show, Eq, Ord, Hashable)

mkStoreName :: FilePath -> Maybe StoreName
mkStoreName path =
  case splitDirectories path of
    "/" : "nix" : "store" : name : _ -> Just . StoreName $ T.pack name
    _ -> Nothing

storeNameToPath :: StoreName -> FilePath
storeNameToPath (StoreName sn) = "/nix/store/" ++ T.unpack sn

--------------------------------------------------------------------------------

data StorePath ref payload
  = StorePath
      { spName :: StoreName,
        spSize :: Int,
        spRefs :: [ref],
        spPayload :: payload
      }
  deriving (Show, Eq, Ord, Functor)

mkStorePaths :: StoreName -> IO [StorePath StoreName ()]
mkStorePaths name = do
  infos <-
    decode @[NixPathInfoResult]
      <$> readProcessStdout_
        ( proc
            "nix"
            ["path-info", "--recursive", "--json", storeNameToPath name]
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

data StoreEnv payload
  = StoreEnv
      { sePaths :: HashMap StoreName (StorePath StoreName payload),
        seTop :: StoreName
      }
  deriving (Functor)

mkStoreEnv :: StoreName -> IO (StoreEnv ())
mkStoreEnv name = do
  paths <- mkStorePaths name
  return $
    StoreEnv
      ( paths
          & map (\p@StorePath {spName} -> (spName, p))
          & HM.fromList
      )
      name

unsafeLookupStoreEnv :: StoreEnv a -> StoreName -> StorePath StoreName a
unsafeLookupStoreEnv StoreEnv {sePaths} name =
  fromMaybe
    (error $ "invariant violation, StoreName not found: " ++ show name)
    (HM.lookup name sePaths)

fetchAllReferences ::
  StoreEnv a ->
  (StoreName -> Bool) ->
  StoreName ->
  [StorePath StoreName a]
fetchAllReferences env predicate = fst . go [] HS.empty
  where
    go acc visited name
      | HS.member name visited = (acc, visited)
      | not (predicate name) = (acc, visited)
      | otherwise =
        let sp@StorePath {spRefs} = unsafeLookupStoreEnv env name
         in foldl'
              (\(acc', visited') name' -> go acc' visited' name')
              (sp : acc, HS.insert name visited)
              spRefs

transformStoreEnv ::
  forall a b.
  (StorePath (StorePath StoreName b) a -> b) ->
  StoreEnv a ->
  StoreEnv b
transformStoreEnv f StoreEnv {sePaths, seTop} =
  StoreEnv
    { sePaths = snd $ execState (go seTop) (sePaths, HM.empty),
      seTop
    }
  where
    unsafeLookup k m =
      fromMaybe
        (error $ "invariant violation: name doesn't exists: " ++ show k)
        (HM.lookup k m)
    go ::
      StoreName ->
      State
        ( HM.HashMap StoreName (StorePath StoreName a),
          HM.HashMap StoreName (StorePath StoreName b)
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

data NixPathInfo
  = NixPathInfo
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
