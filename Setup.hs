-- This Setup.hs converts a Markdown formatted manual to
-- a man page and installs it to the correct location.
--
-- This is first Setup.hs rodeo, so it might not be "correct",
-- but seems to work fine. Let me know if something looks off.
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_, unless)
import Data.Function ((&))
import Data.String (fromString)
import qualified Data.Text.IO as Text
import qualified Distribution.Simple as Cabal
import qualified Distribution.Simple.InstallDirs as Cabal
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Simple.Utils as Cabal
import qualified Distribution.Types.Executable as Cabal
import qualified Distribution.Types.LocalBuildInfo as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Verbosity as Cabal
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import Usage

data Info = Info
  { iExecutableName :: String,
    iProjectName :: String
  }

getInfo :: Cabal.LocalBuildInfo -> IO Info
getInfo lbi = do
  executableName <- case Cabal.executables $ Cabal.localPkgDescr lbi of
    [exe] -> return . Cabal.unUnqualComponentName $ Cabal.exeName exe
    _ -> fail "There must be exactly one executable in the cabal file."

  return
    Info
      { iExecutableName = executableName,
        iProjectName = Cabal.unPackageName $ Cabal.pkgName $ Cabal.package $ Cabal.localPkgDescr lbi
      }

main :: IO ()
main =
  Cabal.defaultMainWithHooks
    Cabal.simpleUserHooks
      { Cabal.postBuild = \_ bf _ lbi -> do
          let docsDir = (Cabal.buildDir lbi) </> "docs"
          Info {iExecutableName} <- getInfo lbi
          let manPath = docsDir </> iExecutableName ++ ".1"

          putStrLn $ "Generating man page: " ++ manPath
          usage <- getUsage
          createDirectoryIfMissing True docsDir
          Text.writeFile manPath (embeddedMan usage)

          return (),
        Cabal.postInst = \_ installFlags _ lbi -> do
          let buildDir = Cabal.buildDir lbi
              absoluteInstallDirs =
                Cabal.absoluteInstallDirs
                  (Cabal.localPackage lbi)
                  (Cabal.localUnitId lbi)
                  (Cabal.compilerInfo . Cabal.compiler $ lbi)
                  (Cabal.fromFlagOrDefault Cabal.NoCopyDest (Cabal.installDest installFlags))
                  (Cabal.hostPlatform lbi)
                  (Cabal.installDirTemplates lbi)
              verbosity = Cabal.fromFlagOrDefault Cabal.normal (Cabal.installVerbosity installFlags)

          let man1Dir = Cabal.mandir absoluteInstallDirs </> "man1"
          Cabal.createDirectoryIfMissingVerbose verbosity True man1Dir

          Cabal.installOrdinaryFile
            verbosity
            (buildDir </> "docs" </> "nix-tree.1")
            (man1Dir </> "nix-tree.1")

          return ()
      }
