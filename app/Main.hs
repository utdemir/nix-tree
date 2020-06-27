{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.List as B
import Control.Concurrent (forkIO)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (filterM, void)
import Data.Function ((&))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Ord (Down (Down))
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Graphics.Vty as V
import PathStats
import System.Directory (canonicalizePath, doesDirectoryExist, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified System.HrfSize as HRF

data Panes
  = PrevPane
  | CurrPane
  | NextPane
  deriving (Show, Eq, Ord)

data Modal
  = HelpModal
  | WhyDependsModal StoreName

data AppEnv = AppEnv
  { aeActualStoreEnv :: StoreEnv PathStats,
    aePrevPane :: List,
    aeCurrPane :: List,
    aeNextPane :: List,
    aeParents :: [List],
    aeOpenModal :: Maybe Modal
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

app :: B.App AppEnv () Panes
app =
  B.App
    { B.appDraw = \env@AppEnv {aeOpenModal} ->
        [ case aeOpenModal of
            Nothing -> B.emptyWidget
            Just HelpModal -> renderHelpModal,
          renderMainScreen env
        ],
      B.appChooseCursor = \_ -> const Nothing,
      B.appHandleEvent = \s e ->
        case aeOpenModal s of
          Nothing ->
            case e of
              B.VtyEvent (V.EvKey k [])
                | k `elem` [V.KChar 'q', V.KEsc] ->
                  B.halt s
              B.VtyEvent (V.EvKey (V.KChar '?') []) ->
                B.continue s {aeOpenModal = Just HelpModal}
              B.VtyEvent (V.EvKey k [])
                | k `elem` [V.KChar 'h', V.KLeft] ->
                  B.continue $ moveLeft s
              B.VtyEvent (V.EvKey k [])
                | k `elem` [V.KChar 'j', V.KDown] ->
                  B.continue $ move B.listMoveDown s
              B.VtyEvent (V.EvKey k [])
                | k `elem` [V.KChar 'k', V.KUp] ->
                  B.continue $ move B.listMoveUp s
              B.VtyEvent (V.EvKey k [])
                | k `elem` [V.KChar 'l', V.KRight] ->
                  B.continue $ moveRight s
              B.VtyEvent (V.EvKey V.KPageUp []) ->
                B.continue =<< moveF B.listMovePageUp s
              B.VtyEvent (V.EvKey V.KPageDown []) ->
                B.continue =<< moveF B.listMovePageDown s
              _ ->
                B.continue s
          Just HelpModal ->
            case e of
              B.VtyEvent (V.EvKey k [])
                | k `elem` [V.KChar 'q', V.KEsc] ->
                  B.continue s {aeOpenModal = Nothing}
              _ ->
                B.continue s,
      B.appStartEvent = \s -> return s,
      B.appAttrMap = \_ ->
        B.attrMap
          (V.white `B.on` V.black)
          [(B.listSelectedFocusedAttr, V.black `B.on` V.white)]
    }

renderMainScreen :: AppEnv -> B.Widget Panes
renderMainScreen env@AppEnv {aePrevPane, aeCurrPane, aeNextPane} =
  (B.joinBorders . B.border)
    ( B.hBox
        [ renderList True aePrevPane,
          B.vBorder,
          renderList True aeCurrPane,
          B.vBorder,
          renderList False aeNextPane
        ]
    )
    B.<=> (B.str . storeNameToPath . spName $ selectedPath env)

renderHelpModal :: B.Widget a
renderHelpModal =
  B.txt helpText
    & B.border
    & B.hLimitPercent 90
    & B.vLimitPercent 60
    & B.centerLayer
  where
    helpText =
      T.intercalate
        "\n"
        [ "hjkl/Arrow Keys : Navigation",
          "q/Esc:          : Quit / Close modal.",
          "w               : why-depends mode.",
          "?               : Show help text."
        ]

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
            B.list CurrPane (S.fromList . NE.toList $ seGetRoots env) 0,
          aeNextPane =
            B.list NextPane S.empty 0,
          aeParents =
            [],
          aeOpenModal =
            Nothing
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
                  . map (seUnsafeLookup aeActualStoreEnv)
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
  paths <- getArgs >>= \case
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
        [] -> fail "No store path given."
        p : ps -> return $ p :| ps
  storePaths <- mapM canonicalizePath paths
  names <- flip mapM storePaths $ \sp ->
    case mkStoreName sp of
      Nothing -> fail $ "Not a store path: " ++ show sp
      Just n -> return n
  env <- calculatePathStats <$> mkStoreEnv names
  run env
