module Data.InvertedIndex
  ( InvertedIndex,
    iiFromList,
    iiInsert,
    iiSearch,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

data InvertedIndex a = InvertedIndex
  { iiElems :: Map Text a,
    iiUnigrams :: Map Char (Set Text),
    iiBigrams :: Map (Char, Char) (Set Text),
    iiTrigrams :: Map (Char, Char, Char) (Set Text)
  }
  deriving (Generic, Show)

instance NFData a => NFData (InvertedIndex a)

iiInsert :: Text -> a -> InvertedIndex a -> InvertedIndex a
iiInsert txt val InvertedIndex {iiElems, iiUnigrams, iiBigrams, iiTrigrams} =
  InvertedIndex
    { iiElems = Map.insert txt val iiElems,
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

iiFromList :: Foldable f => f (Text, a) -> InvertedIndex a
iiFromList =
  foldl'
    (flip (uncurry iiInsert))
    (InvertedIndex Map.empty Map.empty Map.empty Map.empty)

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

iiSearch :: forall a. Text -> InvertedIndex a -> Map Text a
iiSearch txt InvertedIndex {iiElems, iiUnigrams, iiBigrams, iiTrigrams}
  | Text.length txt == 0 = iiElems
  | Text.length txt == 1 = using unigramsOf iiUnigrams
  | Text.length txt == 2 = using bigramsOf iiBigrams
  | otherwise = using trigramsOf iiTrigrams
  where
    lowerTxt = Text.toLower txt
    using :: Ord c => (Text -> Set c) -> Map c (Set Text) -> Map Text a
    using getGrams m =
      Map.intersection m (setToMap () (getGrams txt))
        & Map.elems
        & \case
          [] -> Set.empty
          x : xs -> foldl' Set.intersection x xs
        & Set.filter (\t -> lowerTxt `Text.isInfixOf` Text.toLower t)
        & Map.restrictKeys iiElems
