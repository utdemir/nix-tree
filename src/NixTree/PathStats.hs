module NixTree.PathStats
  ( PathStats (..),
    calculatePathStats,
    whyDepends,
    shortestPathTo,
    module NixTree.StorePath,
  )
where

import Data.List (minimumBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import NixTree.StorePath

data IntermediatePathStats = IntermediatePathStats
  { ipsAllRefs :: M.Map StoreName (StorePath StoreName ())
  }

data PathStats = PathStats
  { psTotalSize :: !Int,
    psAddedSize :: !Int,
    psImmediateParents :: [StoreName],
    psDisambiguationChars :: !Int
  }
  deriving (Show, Generic, NFData)

mkIntermediateEnv ::
  (StoreName -> Bool) ->
  StoreEnv () ->
  StoreEnv IntermediatePathStats
mkIntermediateEnv env =
  seBottomUp $ \curr ->
    IntermediatePathStats
      { ipsAllRefs =
          M.unions
            ( M.fromList
                [ (spName, void sp)
                  | sp@StorePath {spName} <- spRefs curr,
                    env spName
                ]
                : map (ipsAllRefs . spPayload) (spRefs curr)
            )
      }

mkFinalEnv :: StoreEnv IntermediatePathStats -> StoreEnv PathStats
mkFinalEnv env =
  let totalSize = calculateEnvSize env
      immediateParents = calculateImmediateParents (sePaths env)
      disambiguationChars = seDisambiguationChars env
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
                  maybe [] S.toList $ M.lookup spName immediateParents,
                psDisambiguationChars =
                  M.lookup spName disambiguationChars
                    & maybe 0 id
              }
  where
    calculateEnvSize :: StoreEnv IntermediatePathStats -> Int
    calculateEnvSize e =
      seGetRoots e
        & toList
        & map
          ( \sp@StorePath {spName, spPayload} ->
              M.insert
                spName
                (void sp)
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
              (M.fromList (map (,S.singleton spName) spRefs))
        )
        M.empty

    seShortNames :: StoreEnv a -> M.Map Text [StoreName]
    seShortNames env =
      let paths = seAll env & toList
       in foldl'
            ( \m StorePath {spName} ->
                let (_, shortName) = storeNameToSplitShortText spName
                 in M.alter
                      ( \case
                          Nothing -> Just [spName]
                          Just xs -> Just (spName : xs)
                      )
                      shortName
                      m
            )
            M.empty
            paths

    seDisambiguationChars :: StoreEnv a -> M.Map StoreName Int
    seDisambiguationChars env =
      M.toList (seShortNames env)
        & map snd
        & concatMap
          ( \xs ->
              let chrs = disambiguate xs
               in map (\x -> (x, chrs)) xs
          )
        & M.fromList

    disambiguate :: [StoreName] -> Int
    disambiguate xs = go 0
      where
        go n =
          if isGood n
            then n
            else go (n + 2)

        isGood n =
          xs
            & map (storeNameToShortTextWithDisambiguation n)
            & allUnique

        allUnique xx =
          let unique = S.fromList xx
           in length unique == length xx

calculatePathStats :: StoreEnv () -> StoreEnv PathStats
calculatePathStats = mkFinalEnv . mkIntermediateEnv (const True)

-- TODO: This can be precomputed.
shortestPathTo :: StoreEnv a -> StoreName -> NonEmpty (StorePath StoreName a)
shortestPathTo env name =
  seBottomUp
    ( \curr ->
        let currOut = curr {spRefs = spName <$> spRefs curr}
         in if spName curr == name
              then Just (1 :: Int, currOut :| [])
              else
                mapMaybe spPayload (spRefs curr)
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

-- Why depends implementation

-- We iterate the dependency graph bottom up. Every node contains a set of paths which represent
-- the why-depends output from that node down. The set of paths is represented as a "Treeish" object,
-- which is a trie-like structure.
whyDepends :: forall a. StoreEnv a -> StoreName -> [NonEmpty (StorePath StoreName a)]
whyDepends env name =
  seBottomUp @_ @(Maybe (Treeish (StorePath StoreName a)))
    ( \curr ->
        if spName curr == name
          then Just $ mkTreeish (curr {spRefs = map spName (spRefs curr)}) []
          else
            mapMaybe spPayload (spRefs curr)
              & NE.nonEmpty
              <&> NE.toList
              <&> mkTreeish (curr {spRefs = map spName (spRefs curr)})
              <&> capTreeish 1_000_000
    )
    env
    & seGetRoots
    & fmap spPayload
    & NE.toList
    & catMaybes
    & take 10000
    & concatMap treeishToList

-- A trie-like structure which also caches the size.
data Treeish a = Treeish Int a [Treeish a]

mkTreeish :: a -> [Treeish a] -> Treeish a
mkTreeish a ts = Treeish (1 + sum (map (\(Treeish i _ _) -> i) ts)) a ts

treeishSize :: Treeish a -> Int
treeishSize (Treeish i _ _) = i

capTreeish :: Int -> Treeish a -> Treeish a
capTreeish cap (Treeish i a ts)
  | i <= cap = Treeish i a ts
  | otherwise = Treeish cap a (go cap ts)
  where
    go _ [] = []
    go remaining (x : xs) =
      let x' = capTreeish remaining x
          remaining' = remaining - treeishSize x'
       in if remaining > 0
            then x' : go remaining' xs
            else [x']

treeishToList :: Treeish a -> [NonEmpty a]
treeishToList (Treeish _ a []) = [a :| []]
treeishToList (Treeish _ a xs) = map (a NE.<|) (concatMap treeishToList xs)
