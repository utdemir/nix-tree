{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range
import InvertedIndex

prop_inverted_index :: Property
prop_inverted_index = withDiscards 10000 . withTests 10000 . property $ do
  haystack <-
    forAll $
      Gen.set
        (Range.linear 0 100)
        (Gen.text (Range.linear 0 10) Gen.alphaNum)

  needle <-
    forAll $
      (Gen.text (Range.linear 0 5) Gen.alphaNum)

  let ii = iiFromList haystack
  annotateShow ii

  let expected =
        haystack
          & Set.filter
            (\t -> Text.toLower needle `Text.isInfixOf` Text.toLower t)
      actual = iiSearch needle ii

  expected === actual

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
