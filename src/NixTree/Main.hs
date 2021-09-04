module Main where

import NixTree.App
import Control.Concurrent (forkIO)
import qualified Data.HashMap.Strict as HM
import Data.Version (showVersion)
import NixTree.PathStats
import Paths_nix_tree (version)
import System.Directory (canonicalizePath, doesDirectoryExist, getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStr, hPutStrLn)

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
    putStrLn $ "nix-tree " ++ showVersion version
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

    -- Small hack to evaluate the tree branches with a breadth-first
    -- traversal in the background
    let go _ [] = return ()
        go remaining nodes = do
          let (newRemaining, foundNodes) =
                foldl'
                  ( \(nr, fs) n ->
                      ( HM.delete n nr,
                        HM.lookup n nr : fs
                      )
                  )
                  (remaining, [])
                  nodes
          evaluateNF_ foundNodes
          go
            newRemaining
            (concatMap (maybe [] spRefs) foundNodes)

    _ <- forkIO $ go (sePaths env) (toList $ seRoots env)

    run env

  case ret of
    Right () -> return ()
    Left err ->
      usageAndFail $ "Not a store path: " <> show err
