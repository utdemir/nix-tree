{-# LANGUAGE TemplateHaskell #-}

module Test.Data.InvertedIndex where

import Data.InvertedIndex
import qualified Data.Map as Map
import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range

prop_inverted_index :: Property
prop_inverted_index = withDiscards 10000 . withTests 10000 . property $ do
  haystack <-
    forAll $
      Gen.map
        (Range.linear 0 100)
        ( (,)
            <$> Gen.text (Range.linear 0 10) Gen.alphaNum
            <*> Gen.int (Range.linear 0 100)
        )

  needle <-
    forAll $
      (Gen.text (Range.linear 0 5) Gen.alphaNum)

  let ii = iiFromList (Map.toList haystack)
  annotateShow ii

  let expected =
        haystack
          & Map.filterWithKey
            (\t _ -> Text.toLower needle `Text.isInfixOf` Text.toLower t)
      actual = iiSearch needle ii

  expected === actual
