module Clipboard
  ( copy,
  )
where

import Control.Exception (try)
import System.Exit
import qualified System.Process.Typed as P

cmds :: [(FilePath, [String])]
cmds =
  [ ("xsel", ["-i", "-b"]),
    ("xclip", ["-selection", "clipboard"]),
    ("wl-copy", []),
    ("pbcopy", [])
  ]

runCmd :: Text -> (FilePath, [String]) -> IO (Either Text ())
runCmd txt (cmd, args) =
  P.proc (toString cmd) (map toString args)
    & P.setStdin (P.byteStringInput $ encodeUtf8 txt)
    & P.readProcess
    & try
    <&> \case
      (Right (ExitSuccess, _, _)) -> Right ()
      (Right (ExitFailure e, out, err)) ->
        Left $
          "Running " <> show (cmd, args) <> " "
            <> "failed with exit code "
            <> show e
            <> ", "
            <> "stdout: "
            <> decodeUtf8 (toStrict out)
            <> ", "
            <> "stderr: "
            <> decodeUtf8 (toStrict err)
            <> "."
      (Left (ex :: SomeException)) ->
        Left $
          "Running " <> show (cmd, args) <> " "
            <> "failed with exception: "
            <> show ex
            <> "."

copy :: Text -> IO (Either [Text] ())
copy txt = go cmds []
  where
    go [] errs = return $ Left errs
    go (x : xs) errs =
      runCmd txt x >>= \case
        Right () -> return $ Right ()
        Left err -> go xs (err : errs)
