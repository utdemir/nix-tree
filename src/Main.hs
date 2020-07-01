{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.List as B
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Graphics.Vty as V
import PathStats
import Protolude
import System.Directory (canonicalizePath, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))
import qualified System.HrfSize as HRF

data Widgets
  = WidgetPrevPane
  | WidgetCurrPane
  | WidgetNextPane
  | WidgetWhyDepends
  deriving (Show, Eq, Ord)

data Modal s
  = ModalHelp
  | ModalWhyDepends (B.GenericList Widgets Seq (NonEmpty (Path s)))

data AppEnv s = AppEnv
  { aeActualStoreEnv :: StoreEnv s (PathStats s),
    aePrevPane :: List s,
    aeCurrPane :: List s,
    aeNextPane :: List s,
    aeParents :: [List s],
    aeOpenModal :: Maybe (Modal s),
    aeShowInfoPane :: Bool
  }

type Path s = StorePath s (StoreName s) (PathStats s)

type List s = B.GenericList Widgets Seq (Path s)

attrTerminal :: B.AttrName
attrTerminal = "terminal"

run :: StoreEnv s (PathStats s) -> IO ()
run env = void $ B.defaultMain app appEnv
  where
    appEnv =
      AppEnv
        { aeActualStoreEnv =
            env,
          aePrevPane =
            B.list WidgetPrevPane S.empty 0,
          aeCurrPane =
            B.list WidgetCurrPane (S.fromList . NE.toList $ seGetRoots env) 0,
          aeNextPane =
            B.list WidgetNextPane S.empty 0,
          aeParents =
            [],
          aeOpenModal =
            Nothing,
          aeShowInfoPane =
            True
        }
        & repopulateNextPane

renderList ::
  Bool ->
  List s ->
  B.Widget Widgets
renderList isFocused list =
  B.renderList
    ( \_
       StorePath
         { spName,
           spPayload = PathStats {psTotalSize, psAddedSize},
           spRefs
         } ->
          let color =
                if null spRefs
                  then B.withAttr attrTerminal
                  else identity
           in color $
                B.padRight B.Max (B.txt $ storeNameToShortText spName)
                  B.<+> B.padLeft
                    B.Max
                    ( B.txt $
                        prettySize psTotalSize
                          <> if not (null spRefs)
                            then
                              " ("
                                <> prettySize psAddedSize
                                <> ")"
                            else mempty
                    )
    )
    isFocused
    list

app :: B.App (AppEnv s) () Widgets
app =
  B.App
    { B.appDraw = \env@AppEnv {aeOpenModal} ->
        [ case aeOpenModal of
            Nothing -> B.emptyWidget
            Just ModalHelp -> renderHelpModal
            Just (ModalWhyDepends l) -> renderWhyDependsModal l,
          renderMainScreen env
        ],
      B.appChooseCursor = \_ -> const Nothing,
      B.appHandleEvent = \s e ->
        case (e, aeOpenModal s) of
          -- main screen
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'q', V.KEsc] ->
              B.halt s
          (B.VtyEvent (V.EvKey (V.KChar '?') []), Nothing) ->
            B.continue s {aeOpenModal = Just ModalHelp}
          (B.VtyEvent (V.EvKey (V.KChar 'w') []), Nothing) ->
            B.continue $ showWhyDepends s
          (B.VtyEvent (V.EvKey (V.KChar 'i') []), Nothing) ->
            B.continue $ s {aeShowInfoPane = not (aeShowInfoPane s)}
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'h', V.KLeft] ->
              B.continue $ moveLeft s
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'j', V.KDown] ->
              B.continue $ move B.listMoveDown s
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'k', V.KUp] ->
              B.continue $ move B.listMoveUp s
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'l', V.KRight] ->
              B.continue $ moveRight s
          (B.VtyEvent (V.EvKey V.KPageUp []), Nothing) ->
            B.continue =<< moveF B.listMovePageUp s
          (B.VtyEvent (V.EvKey V.KPageDown []), Nothing) ->
            B.continue =<< moveF B.listMovePageDown s
          -- modals
          (B.VtyEvent (V.EvKey k []), Just _)
            | k `elem` [V.KChar 'q', V.KEsc] ->
              B.continue s {aeOpenModal = Nothing}
          -- why-depends modal
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends l))
            | k `elem` [V.KChar 'j', V.KDown] ->
              B.continue s {aeOpenModal = Just $ ModalWhyDepends (B.listMoveDown l)}
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends l))
            | k `elem` [V.KChar 'k', V.KUp] ->
              B.continue s {aeOpenModal = Just $ ModalWhyDepends (B.listMoveUp l)}
          (B.VtyEvent (V.EvKey V.KPageUp []), Just (ModalWhyDepends l)) ->
            B.listMovePageUp l >>= \l' ->
              B.continue s {aeOpenModal = Just $ ModalWhyDepends l'}
          (B.VtyEvent (V.EvKey V.KPageDown []), Just (ModalWhyDepends l)) ->
            B.listMovePageDown l >>= \l' ->
              B.continue s {aeOpenModal = Just $ ModalWhyDepends l'}
          (B.VtyEvent (V.EvKey V.KEnter []), Just (ModalWhyDepends l)) ->
            let closed = s {aeOpenModal = Nothing}
             in case B.listSelectedElement l of
                  Nothing -> B.continue closed
                  Just (_, path) -> B.continue $ selectPath path closed
          -- otherwise
          _ ->
            B.continue s,
      B.appStartEvent = \s -> return s,
      B.appAttrMap = \_ ->
        B.attrMap
          (V.white `B.on` V.black)
          [ (B.listSelectedFocusedAttr, V.black `B.on` V.white),
            (attrTerminal, B.fg V.blue)
          ]
    }

renderMainScreen :: AppEnv s -> B.Widget Widgets
renderMainScreen env@AppEnv {aePrevPane, aeCurrPane, aeNextPane, aeShowInfoPane} =
  (B.joinBorders . B.border)
    ( B.hBox
        [ renderList True aePrevPane,
          B.vBorder,
          renderList True aeCurrPane,
          B.vBorder,
          renderList False aeNextPane
        ]
    )
    B.<=> if aeShowInfoPane then renderInfoPane env else renderModeline env

renderModeline :: AppEnv s -> B.Widget Widgets
renderModeline env =
  let selected = selectedPath env
   in B.txt $
        T.intercalate
          " - "
          [ T.pack $ storeNameToPath (spName selected),
            "NAR Size: " <> prettySize (spSize selected),
            "Closure Size: " <> prettySize (psTotalSize $ spPayload selected)
          ]

renderInfoPane :: AppEnv s -> B.Widget Widgets
renderInfoPane env =
  let selected = selectedPath env
      immediateParents = psImmediateParents $ spPayload selected
   in B.txt $
        T.intercalate
          "\n"
          [ T.pack $ storeNameToPath (spName selected),
            "NAR Size: " <> prettySize (spSize selected),
            "Closure Size: " <> prettySize (psTotalSize $ spPayload selected),
            if null immediateParents
              then "Immediate Parents: -"
              else
                "Immediate Parents (" <> T.pack (show $ length immediateParents) <> "): "
                  <> T.intercalate ", " (map storeNameToShortText immediateParents)
          ]

renderHelpModal :: B.Widget a
renderHelpModal =
  B.txt helpText
    & B.borderWithLabel (B.txt "Help")
    & B.hLimitPercent 90
    & B.vLimitPercent 60
    & B.centerLayer
  where
    helpText =
      T.intercalate
        "\n"
        [ "hjkl/Arrow Keys : Navigate",
          "q/Esc:          : Quit / close modal",
          "w               : Open why-depends mode",
          "i               : Toggle modeline",
          "?               : Show help"
        ]

renderWhyDependsModal ::
  B.GenericList Widgets Seq (NonEmpty (Path s)) ->
  B.Widget Widgets
renderWhyDependsModal l =
  B.renderList renderDepends True l
    & B.hLimitPercent 80
    & B.vLimitPercent 60
    & B.borderWithLabel (B.txt "why-depends")
    & B.centerLayer
  where
    renderDepends _ =
      B.txt . pathsToText
    pathsToText xs =
      xs
        & NE.toList
        & fmap (storeNameToShortText . spName)
        & T.intercalate " → "

showWhyDepends :: AppEnv s -> AppEnv s
showWhyDepends env@AppEnv {aeActualStoreEnv} =
  env
    { aeOpenModal =
        Just . ModalWhyDepends $
          let selected = selectedPath env
              route = selectedPaths env
              xs = S.fromList $ whyDepends aeActualStoreEnv (spName selected)
           in B.list WidgetWhyDepends xs 1
                & B.listMoveTo
                  (fromMaybe 0 $ (((==) `on` fmap spName) route) `S.findIndexL` xs)
    }

move :: (List s -> List s) -> AppEnv s -> AppEnv s
move f = runIdentity . moveF (Identity . f)

moveF :: Applicative f => (List s -> f (List s)) -> AppEnv s -> f (AppEnv s)
moveF f env@AppEnv {aeCurrPane} =
  repopulateNextPane . (\p -> env {aeCurrPane = p}) <$> f aeCurrPane

moveLeft :: AppEnv s -> AppEnv s
moveLeft env@AppEnv {aeParents = []} = env
moveLeft env@AppEnv {aePrevPane, aeCurrPane, aeParents = parent : grandparents} =
  env
    { aeParents = grandparents,
      aePrevPane = parent,
      aeCurrPane = aePrevPane {B.listName = WidgetCurrPane},
      aeNextPane = aeCurrPane {B.listName = WidgetNextPane}
    }

moveRight :: AppEnv s -> AppEnv s
moveRight env@AppEnv {aePrevPane, aeCurrPane, aeNextPane, aeParents}
  | null (B.listElements aeNextPane) = env
  | otherwise =
    env
      { aePrevPane = aeCurrPane {B.listName = WidgetPrevPane},
        aeCurrPane = aeNextPane {B.listName = WidgetCurrPane},
        aeParents = aePrevPane : aeParents
      }
      & repopulateNextPane

repopulateNextPane :: AppEnv s -> AppEnv s
repopulateNextPane env@AppEnv {aeActualStoreEnv, aeNextPane} =
  let ref = selectedPath env
   in env
        { aeNextPane =
            B.listReplace
              ( S.sortOn (Down . psTotalSize . spPayload)
                  . S.fromList
                  . map (seLookup aeActualStoreEnv)
                  $ spRefs ref
              )
              (Just 0)
              aeNextPane
        }

selectedPath :: AppEnv s -> Path s
selectedPath = NE.head . selectedPaths

selectedPaths :: AppEnv s -> NonEmpty (Path s)
selectedPaths AppEnv {aePrevPane, aeCurrPane, aeParents} =
  let parents =
        mapMaybe
          (fmap snd . B.listSelectedElement)
          (aePrevPane : aeParents)
   in case B.listSelectedElement aeCurrPane of
        Nothing -> panic "invariant violation: no selected element"
        Just (_, p) -> p :| parents

selectPath :: NonEmpty (Path s) -> AppEnv s -> AppEnv s
selectPath path env
  | (spName <$> path) == (spName <$> selectedPaths env) =
    env
selectPath path env@AppEnv {aeActualStoreEnv} =
  let root :| children = NE.reverse path
      lists =
        NE.scanl
          ( \(_, prev) curr ->
              ( map (seLookup aeActualStoreEnv) $
                  spRefs prev,
                curr
              )
          )
          (NE.toList (seGetRoots aeActualStoreEnv), root)
          children
          & NE.reverse
          & fmap
            (\(possible, selected) -> mkList WidgetPrevPane possible selected)
          & (<> (emptyPane :| []))
   in case lists of
        (curr :| prevs) ->
          let (prev, parents) = case prevs of
                [] -> (emptyPane, [])
                p : ps -> (p, ps)
           in env
                { aeParents = parents,
                  aePrevPane = prev,
                  aeCurrPane = curr {B.listName = WidgetCurrPane}
                }
                & repopulateNextPane
  where
    mkList name possible selected =
      let contents = S.sortOn (Down . psTotalSize . spPayload) (S.fromList possible)
       in B.list name contents 1
            & B.listMoveTo
              (fromMaybe (0) $ (((==) `on` spName) selected) `S.findIndexL` contents)
    emptyPane =
      B.list WidgetPrevPane S.empty 0

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
        [] -> panic "No store path given."
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
          evaluate $ rnf foundNodes
          go
            newRemaining
            (concatMap (maybe [] spRefs) foundNodes)
    _ <- forkIO $ go (sePaths env) (NE.toList $ seRoots env)

    run env

  case ret of
    Right () -> return ()
    Left err ->
      mapM_
        (\fp -> hPutStrLn stderr $ "Not a store path: " <> fp)
        err

-- Utils

prettySize :: Int -> T.Text
prettySize size = case HRF.convertSize $ fromIntegral size of
  HRF.Bytes d -> T.pack (show d)
  HRF.KiB d -> T.pack (show d) <> " KiB"
  HRF.MiB d -> T.pack (show d) <> " MiB"
  HRF.GiB d -> T.pack (show d) <> " GiB"
  HRF.TiB d -> T.pack (show d) <> " TiB"
