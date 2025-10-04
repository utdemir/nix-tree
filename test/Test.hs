{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import qualified Test.NixTree.Data.InvertedIndex

main :: IO ()
main =
  defaultMain . map checkParallel $
    [Test.NixTree.Data.InvertedIndex.tests]
