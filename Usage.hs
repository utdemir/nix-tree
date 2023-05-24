{-# LANGUAGE TemplateHaskell #-}

module Usage where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Language.Haskell.TH
import qualified Text.Pandoc as Pandoc
import Language.Haskell.TH.Syntax (Quasi(qAddDependentFile), qRunIO)

data EmbeddedUsage = EmbeddedUsage
  { embeddedMan :: Text.Text,
    embeddedTxt :: Text.Text
  }

const_MANUAL_FILE = "./MANUAL.md"

getUsage :: IO EmbeddedUsage
getUsage = do
  contents <- Text.readFile const_MANUAL_FILE
  input <- Pandoc.runIOorExplode $ Pandoc.readMarkdown Pandoc.def contents
  man <- Pandoc.runIOorExplode $ Pandoc.writeMan Pandoc.def input
  txt <- Pandoc.runIOorExplode $ Pandoc.writePlain Pandoc.def input
  return $ EmbeddedUsage { embeddedMan = man, embeddedTxt = txt }

embedUsage :: Q Exp
embedUsage = do
  qAddDependentFile const_MANUAL_FILE
  qRunIO $ do
    usage <- getUsage
    return $
        RecConE
        'EmbeddedUsage
        [ ('embeddedMan, AppE (VarE 'Text.pack) (LitE (StringL (Text.unpack (embeddedMan usage))))),
            ('embeddedTxt, AppE (VarE 'Text.pack) (LitE (StringL (Text.unpack (embeddedTxt usage)))))
        ]
