{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent.Async
import Control.Exception (evaluate)
import NixTree.App
import NixTree.PathStats
import System.Directory (canonicalizePath, doesDirectoryExist, getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStr, hPutStrLn)
import System.ProgressBar hiding (msg)

version ::  Text
version = VERSION_nix_tree

usage :: Text
usage =
  unlines
    [ "Usage: nix-tree [paths...] [-h|--help] [--version]",
      "  Paths default to $HOME/.nix-profile and /var/run/current-system.",
      "Keybindings:",
      unlines . map ("  " <>) . lines $ helpText
    ]

usageAndFail :: Text -> IO a
usageAndFail msg = do
  hPutStrLn stderr . toString $ "Error: " <> msg
  hPutStr stderr $ toString usage
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  when (any (`elem` ["-h", "--help"]) args) $ do
    putText usage
    exitWith ExitSuccess

  when ("--version" `elem` args) $ do
    putTextLn $ "nix-tree " <> version
    exitWith ExitSuccess

  paths <- case args of
    p : ps ->
      return $ p :| ps
    [] -> do
      home <- getHomeDirectory
      roots <-
        filterM
          doesDirectoryExist
          [ home </> ".nix-profile",
            "/var/run/current-system"
          ]
      case roots of
        [] -> usageAndFail "No store path given."
        p : ps -> return $ p :| ps
  storePaths <- mapM canonicalizePath paths
  ret <- withStoreEnv storePaths $ \env' -> do
    let env = calculatePathStats env'
        allPaths = seAll env

    bar <- newProgressBar defStyle {stylePostfix = exact} 4 (Progress 0 (length allPaths) ())
    allPaths
      & toList
      & chunks 50
      & mapConcurrently_ (mapM_ (\p -> evaluate (rnf p) >> incProgress bar 1))

    run env

  case ret of
    Right () -> return ()
    Left err ->
      usageAndFail $ "Not a store path: " <> show err

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs
