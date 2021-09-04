{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import qualified Test.Data.InvertedIndex

main :: IO ()
main =
  defaultMain . map checkParallel $
    [Test.Data.InvertedIndex.tests]
