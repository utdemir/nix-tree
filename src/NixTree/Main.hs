{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent.Async
import Control.Exception (evaluate)
import NixTree.App
import NixTree.PathStats
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import System.ProgressBar hiding (msg)

version :: Text
version = VERSION_nix_tree

data Opts = Opts
  { oInstallables :: [Installable],
    oVersion :: Bool,
    oDerivation :: Bool,
    oImpure :: Bool
  }

optsParser :: Opts.ParserInfo Opts
optsParser =
  Opts.info (parser <**> Opts.helper) $
    mconcat
      [ Opts.progDesc "Interactively browse dependency graphs of Nix derivations.",
        Opts.fullDesc,
        Opts.footerDoc (Just keybindingsHelp)
      ]
  where
    parser :: Opts.Parser Opts
    parser =
      Opts
        <$> many
          ( Installable
              <$> Opts.strArgument @Text
                ( Opts.metavar "INSTALLABLE"
                    <> Opts.helpDoc
                      ( Just $
                          "A store path or a flake reference."
                            Opts.<$$> "Paths default to \"~/.nix-profile\" and \"/var/run/current-system\""
                      )
                )
          )
        <*> Opts.switch (Opts.long "version" <> Opts.help "Show the nix-tree version")
        <*> Opts.switch (Opts.long "derivation" <> Opts.help "Operate on the store derivation rather than its outputs")
        <*> Opts.switch (Opts.long "impure" <> Opts.help "Allow access to mutable paths and repositories")

    keybindingsHelp :: Opts.Doc
    keybindingsHelp =
      "Keybindings:"
        Opts.<$$> (Opts.indent 2 . Opts.vsep $ map (Opts.text . toString) (lines helpText))

showAndFail :: Text -> IO a
showAndFail msg = do
  hPutStrLn stderr . toString $ "Error: " <> msg
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  opts <-
    Opts.customExecParser
      (Opts.prefs $ Opts.columns 120)
      optsParser

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
        [] -> showAndFail "No store path given."
        p : ps -> return . fmap (Installable . toText) $ p :| ps

  let seo =
        StoreEnvOptions
          { seoIsDerivation = opts & oDerivation,
            seoIsImpure = opts & oImpure
          }

  withStoreEnv seo installables $ \env' -> do
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
