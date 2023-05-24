{-# LANGUAGE TemplateHaskell #-}

module NixTree.Man where

import Control.Exception (try)
import qualified Data.Text.IO as Text
import System.FilePath ((</>))
import System.IO.Temp
import qualified System.Process.Typed as P
import Usage

embeddedUsage :: EmbeddedUsage
embeddedUsage = $(embedUsage)

spawnManualWithMan :: IO Bool
spawnManualWithMan = do
  withSystemTempDirectory "nix-tree" $ \dir -> do
    let manPath = dir </> "nix-tree.1"
    Text.writeFile manPath (embeddedMan embeddedUsage)
    ret <-
      try @SomeException $ P.runProcess (P.proc "man" [manPath])
    return $ isRight ret

spawnManualWithLess :: IO Bool
spawnManualWithLess = do
  ret <-
    try @SomeException
      $ P.proc "less" []
        & P.setStdin (P.byteStringInput (toLazy . encodeUtf8 $ embeddedTxt embeddedUsage))
        & P.runProcess
  return $ isRight ret

printManual :: IO ()
printManual = Text.putStrLn (embeddedTxt embeddedUsage)

showManualSomehow :: IO ()
showManualSomehow = 
  spawnManualWithMan >>= \case
    True -> return ()
    False -> spawnManualWithLess >>= \case
      True -> return ()
      False -> printManual