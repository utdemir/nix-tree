module Clipboard
  ( copy,
  )
where

import qualified Data.ByteString.Lazy as BL
import qualified System.Process.Typed as P

cmds :: [(Text, [Text])]
cmds =
  [ ("xsel", ["-i", "-b"]),
    ("xclip", ["-selection", "clipboard"]),
    ("wl-copy", []),
    ("pbcopy", [])
  ]

runCmd :: Text -> (Text, [Text]) -> IO (Either Text ())
runCmd txt (cmd, args) =
  P.proc (toS cmd) (map toS args)
    & P.setStdin (P.byteStringInput $ toUtf8Lazy txt)
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
            <> decodeUtf8 (BL.toStrict out)
            <> ", "
            <> "stderr: "
            <> decodeUtf8 (BL.toStrict err)
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
