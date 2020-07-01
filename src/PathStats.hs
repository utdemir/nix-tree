{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PathStats
  ( PathStats (..),
    calculatePathStats,
    markRouteTo,
    whyDepends,
    module StorePath,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Protolude
import StorePath

data IntermediatePathStats = IntermediatePathStats
  { ipsAllRefs :: M.Map StoreName (StorePath StoreName ())
  }

data PathStats = PathStats
  { psTotalSize :: !Int,
    psAddedSize :: !Int,
    psImmediateParents :: [StoreName]
  }
  deriving (Show, Generic, NFData)

mkIntermediateEnv ::
  (StoreName -> Bool) ->
  StoreEnv () ->
  StoreEnv IntermediatePathStats
mkIntermediateEnv pred =
  seBottomUp $ \curr ->
    IntermediatePathStats
      { ipsAllRefs =
          M.unions
            ( M.fromList
                [ (spName, const () <$> sp)
                  | sp@StorePath {spName} <- spRefs curr,
                    pred spName
                ]
                : map (ipsAllRefs . spPayload) (spRefs curr)
            )
      }

mkFinalEnv :: StoreEnv IntermediatePathStats -> StoreEnv PathStats
mkFinalEnv env =
  let totalSize = calculateEnvSize env
      immediateParents = calculateImmediateParents (sePaths env)
   in flip seBottomUp env $ \StorePath {spName, spSize, spPayload} ->
        let filteredSize =
              seFetchRefs env (/= spName) (seRoots env)
                & calculateRefsSize
            addedSize = totalSize - filteredSize
         in PathStats
              { psTotalSize =
                  spSize
                    + calculateRefsSize (ipsAllRefs spPayload),
                psAddedSize = addedSize,
                psImmediateParents =
                  maybe [] S.toList $ M.lookup spName immediateParents
              }
  where
    calculateEnvSize :: StoreEnv IntermediatePathStats -> Int
    calculateEnvSize env =
      seGetRoots env
        & toList
        & map
          ( \sp@StorePath {spName, spPayload} ->
              M.insert
                spName
                (const () <$> sp)
                (ipsAllRefs spPayload)
          )
        & M.unions
        & calculateRefsSize
    calculateRefsSize :: (Functor f, Foldable f) => f (StorePath a b) -> Int
    calculateRefsSize = sum . fmap spSize
    calculateImmediateParents ::
      (Foldable f) =>
      f (StorePath StoreName b) ->
      M.Map StoreName (S.Set StoreName)
    calculateImmediateParents =
      foldl'
        ( \m StorePath {spName, spRefs} ->
            M.unionWith
              (<>)
              m
              (M.fromList (map (\r -> (r, S.singleton spName)) spRefs))
        )
        M.empty

calculatePathStats :: StoreEnv () -> StoreEnv PathStats
calculatePathStats = mkFinalEnv . mkIntermediateEnv (const True)

whyDepends :: StoreEnv a -> StoreName -> [NonEmpty (StorePath StoreName a)]
whyDepends env name =
  seBottomUp
    ( \curr ->
        if spName curr == name
          then [curr {spRefs = map spName (spRefs curr)} :| []]
          else
            concat . transpose $
              map
                (map (curr {spRefs = map spName (spRefs curr)} NE.<|) . spPayload)
                (spRefs curr)
    )
    env
    & seGetRoots
    & fmap spPayload
    & concat
    & map NE.reverse

markRouteTo :: StoreName -> StoreEnv a -> StoreEnv (Bool, a)
markRouteTo name = seBottomUp $ \sp@StorePath {spName, spRefs} ->
  ( spName == name || any (fst . spPayload) spRefs,
    spPayload sp
  )
