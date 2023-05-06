{-# LANGUAGE TemplateHaskell #-}

module Usage where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Language.Haskell.TH
import qualified Text.Pandoc as Pandoc
import Language.Haskell.TH.Syntax (Quasi(qAddDependentFile), qRunIO)

data EmbeddedUsage = EmbeddedUsage
  { embeddedMan :: Text,
    embeddedTxt :: Text
  }

embedUsage :: Q Exp
embedUsage = do
  qAddDependentFile "./usage.md"
  qRunIO $ do
    input <-
        Text.readFile "./usage.md"
        >>= Pandoc.runIOorExplode . Pandoc.readMarkdown Pandoc.def
    man <- Pandoc.runIOorExplode $ Text.unpack <$> Pandoc.writeMan Pandoc.def input
    txt <- Pandoc.runIOorExplode $ Text.unpack <$> Pandoc.writePlain Pandoc.def input
    return $
        RecConE
        'EmbeddedUsage
        [ ('embeddedMan, AppE (VarE 'Text.pack) (LitE (StringL man))),
            ('embeddedTxt, AppE (VarE 'Text.pack) (LitE (StringL txt)))
        ]
