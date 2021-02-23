module PathStats
  ( PathStats (..),
    calculatePathStats,
    whyDepends,
    shortestPathTo,
    module StorePath,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import StorePath

data IntermediatePathStats s = IntermediatePathStats
  { ipsAllRefs :: M.Map (StoreName s) (StorePath s (StoreName s) ())
  }

data PathStats s = PathStats
  { psTotalSize :: !Int,
    psAddedSize :: !Int,
    psImmediateParents :: [StoreName s]
  }
  deriving (Show, Generic, NFData)

mkIntermediateEnv ::
  (StoreName s -> Bool) ->
  StoreEnv s () ->
  StoreEnv s (IntermediatePathStats s)
mkIntermediateEnv env =
  seBottomUp $ \curr ->
    IntermediatePathStats
      { ipsAllRefs =
          M.unions
            ( M.fromList
                [ (spName, const () <$> sp)
                  | sp@StorePath {spName} <- spRefs curr,
                    env spName
                ] :
              map (ipsAllRefs . spPayload) (spRefs curr)
            )
      }

mkFinalEnv :: StoreEnv s (IntermediatePathStats s) -> StoreEnv s (PathStats s)
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
    calculateEnvSize :: StoreEnv s (IntermediatePathStats s) -> Int
    calculateEnvSize e =
      seGetRoots e
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
    calculateRefsSize :: (Functor f, Foldable f) => f (StorePath s a b) -> Int
    calculateRefsSize = sum . fmap spSize
    calculateImmediateParents ::
      (Foldable f) =>
      f (StorePath s (StoreName s) b) ->
      M.Map (StoreName s) (S.Set (StoreName s))
    calculateImmediateParents =
      foldl'
        ( \m StorePath {spName, spRefs} ->
            M.unionWith
              (<>)
              m
              (M.fromList (map (\r -> (r, S.singleton spName)) spRefs))
        )
        M.empty

calculatePathStats :: StoreEnv s () -> StoreEnv s (PathStats s)
calculatePathStats = mkFinalEnv . mkIntermediateEnv (const True)

whyDepends :: StoreEnv s a -> StoreName s -> [NonEmpty (StorePath s (StoreName s) a)]
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

-- TODO: This can be precomputed.
shortestPathTo :: StoreEnv s a -> StoreName s -> NonEmpty (StorePath s (StoreName s) a)
shortestPathTo env name =
  seBottomUp
    ( \curr ->
        let currOut = curr {spRefs = spName <$> spRefs curr}
         in if spName curr == name
              then Just (1 :: Int, currOut :| [])
              else
                spRefs curr
                  & fmap spPayload
                  & catMaybes
                  & \case
                    [] -> Nothing
                    xs -> case minimumBy (comparing fst) xs of
                      (c, p) -> Just (c + 1, currOut NE.<| p)
    )
    env
    & seGetRoots
    & fmap spPayload
    & NE.toList
    & catMaybes
    & minimumBy (comparing fst)
    & snd
    & NE.reverse
