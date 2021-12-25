{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent.Async
import Control.Exception (evaluate)
import NixTree.App
import NixTree.PathStats
import qualified Options.Applicative as Opts
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStr, hPutStrLn)
import System.ProgressBar hiding (msg)

version :: Text
version = VERSION_nix_tree

data Opts = Opts
  { oVersion :: Bool,
    oDerivation :: Bool,
    oInstallables :: [Installable]
  }

optsParser :: Opts.ParserInfo Opts
optsParser =
  Opts.info (parser <**> Opts.helper) $
    mconcat
      [ Opts.progDesc "Interactively browse dependency graphs of Nix derivations.",
        Opts.fullDesc
      ]
  where
    parser :: Opts.Parser Opts
    parser =
      Opts
        <$> Opts.switch (Opts.long "version" <> Opts.help "Show the nix-tree version.")
        <*> Opts.switch (Opts.long "derivation" <> Opts.help "Operate on the store derivation rather than its outputs.")
        <*> many (Opts.strArgument @Text (Opts.metavar "INSTALLABLE" <> Opts.help "A store path or a flake reference.") <&> Installable)

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
  opts <- Opts.execParser optsParser

  when (opts & oVersion) $ do
    putTextLn $ "nix-tree " <> version
    exitWith ExitSuccess

  installables <- case opts & oInstallables of
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
        p : ps -> return . fmap (Installable . toText) $ p :| ps

  withStoreEnv (opts & oDerivation) installables $ \env' -> do
    let env = calculatePathStats env'
        allPaths = seAll env

    bar <- newProgressBar defStyle {stylePostfix = exact} 4 (Progress 0 (length allPaths) ())
    allPaths
      & toList
      & chunks 50
      & mapConcurrently_ (mapM_ (\p -> evaluate (rnf p) >> incProgress bar 1))

    run env

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs
