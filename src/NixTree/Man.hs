{-# LANGUAGE TemplateHaskell #-}

module NixTree.Man where

import qualified Data.ByteString as BS
import Data.FileEmbed
import qualified Paths_nix_tree
import System.FilePath ((</>))
import System.IO.Temp
import qualified System.Process.Typed as P

manContents :: ByteString
manContents = $(dummySpaceWith "man" $ 1024 * 8)

txtContents :: ByteString
txtContents = $(dummySpaceWith "txt" $ 1024 * 8)

spawnManual :: IO ()
spawnManual = do
  withSystemTempDirectory "nix-tree" $ \dir -> do
    let manPath = dir </> "nix-tree.1"
    BS.writeFile manPath manContents
    _ret <-
      P.proc "man" [manPath]
        & P.runProcess
    return ()
