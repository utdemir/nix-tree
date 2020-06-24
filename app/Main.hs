{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import Control.Monad (void)
import Data.Function ((&))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (Down (Down))
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Graphics.Vty as V
import PathStats
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import qualified System.HrfSize as HRF

data Panes
  = PrevPane
  | CurrPane
  | NextPane
  deriving (Show, Eq, Ord)

data AppEnv
  = AppEnv
      { aeActualStoreEnv :: StoreEnv PathStats,
        aePrevPane :: List,
        aeCurrPane :: List,
        aeNextPane :: List,
        aeParents :: [List]
      }

type Path = StorePath StoreName PathStats

type List = B.GenericList Panes Seq Path

renderList ::
  Bool ->
  List ->
  B.Widget Panes
renderList isFocused list =
  B.renderList
    ( \_
       StorePath
         { spName = StoreName txt,
           spPayload = PathStats {psTotalSize, psAddedSize}
         } ->
          let prettyName = T.drop 1 . T.dropWhile (/= '-') $ txt
              prettySize size = case HRF.convertSize $ fromIntegral size of
                HRF.Bytes d -> T.pack (show d)
                HRF.KiB d -> T.pack (show d) <> " KiB"
                HRF.MiB d -> T.pack (show d) <> " MiB"
                HRF.GiB d -> T.pack (show d) <> " GiB"
                HRF.TiB d -> T.pack (show d) <> " TiB"
           in B.padRight B.Max (B.txt prettyName)
                B.<+> B.padLeft
                  B.Max
                  ( B.txt $
                      prettySize psTotalSize
                        <> " ("
                        <> prettySize psAddedSize
                        <> ")"
                  )
    )
    isFocused
    list
    & B.hLimit 60

app :: B.App AppEnv () Panes
app =
  B.App
    { B.appDraw = \env@AppEnv {aePrevPane, aeCurrPane, aeNextPane} ->
        [ ( (B.border $ renderList True aePrevPane)
              B.<+> (B.border $ renderList True aeCurrPane)
              B.<+> (B.border $ renderList False aeNextPane)
          )
            B.<=> (B.str . storeNameToPath . spName $ selectedPath env)
        ],
      B.appChooseCursor = \_ -> const Nothing,
      B.appHandleEvent = \s -> \case
        B.VtyEvent (V.EvKey V.KEsc []) -> B.halt s
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt s
        B.VtyEvent (V.EvKey (V.KChar 'h') []) -> B.continue $ moveLeft s
        B.VtyEvent (V.EvKey (V.KChar 'j') []) -> B.continue $ move B.listMoveDown s
        B.VtyEvent (V.EvKey (V.KChar 'k') []) -> B.continue $ move B.listMoveUp s
        B.VtyEvent (V.EvKey (V.KChar 'l') []) -> B.continue $ moveRight s
        B.VtyEvent (V.EvKey V.KPageUp []) -> B.continue =<< moveF B.listMovePageUp s
        B.VtyEvent (V.EvKey V.KPageDown []) -> B.continue =<< moveF B.listMovePageDown s
        _ -> B.continue s,
      B.appStartEvent = \s -> return s,
      B.appAttrMap = \_ ->
        B.attrMap
          (V.white `B.on` V.black)
          [(B.listSelectedFocusedAttr, V.black `B.on` V.white)]
    }

run :: StoreEnv PathStats -> IO ()
run env = void $ B.defaultMain app appEnv
  where
    appEnv =
      AppEnv
        { aeActualStoreEnv =
            env,
          aePrevPane =
            B.list PrevPane S.empty 0,
          aeCurrPane =
            B.list CurrPane (S.singleton . unsafeLookupStoreEnv env $ seTop env) 0,
          aeNextPane =
            B.list NextPane S.empty 0,
          aeParents =
            []
        }
        & repopulateSecondPane

move :: (List -> List) -> AppEnv -> AppEnv
move f = runIdentity . moveF (Identity . f)

moveF :: Applicative f => (List -> f List) -> AppEnv -> f AppEnv
moveF f env@AppEnv {aeCurrPane} =
  repopulateSecondPane . (\p -> env {aeCurrPane = p}) <$> f aeCurrPane

moveLeft :: AppEnv -> AppEnv
moveLeft env@AppEnv {aeParents = []} = env
moveLeft env@AppEnv {aePrevPane, aeCurrPane, aeParents = parent : grandparents} =
  env
    { aeParents = grandparents,
      aePrevPane = parent,
      aeCurrPane = aePrevPane {B.listName = CurrPane},
      aeNextPane = aeCurrPane {B.listName = NextPane}
    }

moveRight :: AppEnv -> AppEnv
moveRight env@AppEnv {aePrevPane, aeCurrPane, aeNextPane, aeParents}
  | null (B.listElements aeNextPane) = env
  | otherwise =
    env
      { aePrevPane = aeCurrPane {B.listName = PrevPane},
        aeCurrPane = aeNextPane {B.listName = CurrPane},
        aeParents = aePrevPane : aeParents
      }
      & repopulateSecondPane

repopulateSecondPane :: AppEnv -> AppEnv
repopulateSecondPane env@AppEnv {aeActualStoreEnv, aeNextPane} =
  let ref = selectedPath env
   in env
        { aeNextPane =
            B.listReplace
              ( S.sortOn (Down . psTotalSize . spPayload)
                  . S.fromList
                  . map (unsafeLookupStoreEnv aeActualStoreEnv)
                  $ spRefs ref
              )
              (Just 0)
              aeNextPane
        }

selectedPath :: AppEnv -> Path
selectedPath AppEnv {aeCurrPane} =
  case B.listSelectedElement aeCurrPane of
    Nothing -> error "invariant violation: no selected element"
    Just (_, sn) -> sn

main :: IO ()
main = do
  getArgs >>= \case
    [storePath] -> do
      canon <- canonicalizePath storePath
      env <- case mkStoreName canon of
        Nothing -> fail $ "Not a store path: " ++ show storePath
        Just xs -> do
          putStrLn $ "Gathering information..."
          calculatePathStats <$> mkStoreEnv xs
      putStrLn $
        "  Total size: "
          ++ show (psTotalSize . spPayload $ unsafeLookupStoreEnv env (seTop env))
      putStrLn $ "Running..."
      run env
      putStrLn $ "Done."
    _ -> fail "invalid syntax."
