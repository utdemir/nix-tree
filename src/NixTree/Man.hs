{-# LANGUAGE TemplateHaskell #-}

module NixTree.Man where

import qualified Data.Text.IO as Text
import System.FilePath ((</>))
import System.IO.Temp
import qualified System.Process.Typed as P
import Usage

embedded :: EmbeddedUsage
embedded = $(embedUsage)

spawnManual :: IO ()
spawnManual = do
  withSystemTempDirectory "nix-tree" $ \dir -> do
    let manPath = dir </> "nix-tree.1"
    Text.writeFile manPath (embeddedMan embedded)
    _ret <-
      P.proc "man" [manPath]
        & P.runProcess
    return ()
