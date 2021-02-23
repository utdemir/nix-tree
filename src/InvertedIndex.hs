module InvertedIndex
  ( InvertedIndex,
    iiFromList,
    iiInsert,
    iiSearch,
  )
where

import Data.List (zip3)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

data InvertedIndex = InvertedIndex
  { iiElems :: Set Text,
    iiUnigrams :: Map Char (Set Text),
    iiBigrams :: Map (Char, Char) (Set Text),
    iiTrigrams :: Map (Char, Char, Char) (Set Text)
  }
  deriving (Generic, Show)

instance NFData InvertedIndex

iiInsert :: Text -> InvertedIndex -> InvertedIndex
iiInsert txt InvertedIndex {iiElems, iiUnigrams, iiBigrams, iiTrigrams} =
  InvertedIndex
    { iiElems = Set.insert txt iiElems,
      iiUnigrams = combine iiUnigrams (unigramsOf txt),
      iiBigrams = combine iiBigrams (bigramsOf txt),
      iiTrigrams = combine iiTrigrams (trigramsOf txt)
    }
  where
    combine orig chrs =
      Map.unionWith
        (<>)
        orig
        (setToMap (Set.singleton txt) chrs)

iiFromList :: Foldable f => f Text -> InvertedIndex
iiFromList =
  foldl
    (flip iiInsert)
    (InvertedIndex Set.empty Map.empty Map.empty Map.empty)

setToMap :: v -> Set k -> Map k v
setToMap v = Map.fromDistinctAscList . map (,v) . Set.toAscList

unigramsOf :: Text -> Set Char
unigramsOf = Set.fromList . Text.unpack . Text.toLower

bigramsOf :: Text -> Set (Char, Char)
bigramsOf txt = case Text.unpack (Text.toLower txt) of
  p1@(_ : p2) -> Set.fromList $ zip p1 p2
  _ -> Set.empty

trigramsOf :: Text -> Set (Char, Char, Char)
trigramsOf txt = case Text.unpack (Text.toLower txt) of
  p1@(_ : p2@(_ : p3)) -> Set.fromList $ zip3 p1 p2 p3
  _ -> Set.empty

iiSearch :: Text -> InvertedIndex -> Set Text
iiSearch txt InvertedIndex {iiElems, iiUnigrams, iiBigrams, iiTrigrams}
  | Text.length txt == 0 = iiElems
  | Text.length txt == 1 = using unigramsOf iiUnigrams
  | Text.length txt == 2 = using bigramsOf iiBigrams
  | otherwise = using trigramsOf iiTrigrams
  where
    lowerTxt = Text.toLower txt
    using :: Ord a => (Text -> Set a) -> Map a (Set Text) -> Set Text
    using getGrams m =
      Map.intersection m (setToMap () (getGrams txt))
        & Map.elems
        & \case
          [] -> Set.empty
          x : xs -> foldl' Set.intersection x xs
        & Set.filter (\t -> lowerTxt `Text.isInfixOf` Text.toLower t)
