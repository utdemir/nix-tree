module NixTree.App (run, helpText) where

import qualified Brick as B
import qualified Brick.BChan as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.List as B
import Control.Concurrent
import Data.InvertedIndex
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro (Traversal', _Just)
import qualified NixTree.Clipboard as Clipboard
import NixTree.PathStats
import qualified System.Clock as Clock
import qualified System.HrfSize as HRF

sortOrderChangeHighlightPeriod :: Clock.TimeSpec
sortOrderChangeHighlightPeriod = Clock.TimeSpec 0 (500 * 1_000_000)

data Event
  = EventTick Clock.TimeSpec

data Widgets
  = WidgetPrevPane
  | WidgetCurrPane
  | WidgetNextPane
  | WidgetWhyDepends
  | WidgetSearch
  | WidgetWhyDependsViewport
  deriving (Show, Eq, Ord)

data Notice = Notice Text Text

data Modal s
  = ModalNotice Notice
  | ModalWhyDepends (B.GenericList Widgets Seq (NonEmpty Path))
  | ModalSearch Text Text (B.GenericList Widgets Seq Path)

succCycle :: forall a. (Bounded a, Enum a) => a -> a
succCycle a
  | fromEnum a == fromEnum (maxBound @a) = minBound
  | otherwise = succ a

data AppEnv s = AppEnv
  { aeActualStoreEnv :: StoreEnv PathStats,
    aeInvertedIndex :: InvertedIndex Path,
    aePrevPane :: List,
    aeCurrPane :: List,
    aeNextPane :: List,
    aeParents :: [List],
    aeOpenModal :: Maybe (Modal s),
    aeSortOrder :: SortOrder,
    aeSortOrderLastChanged :: Clock.TimeSpec,
    aeCurrTime :: Clock.TimeSpec
  }

type Path = StorePath StoreName PathStats

type List = B.GenericList Widgets Seq Path

data SortOrder
  = SortOrderAlphabetical
  | SortOrderClosureSize
  | SortOrderAddedSize
  deriving (Show, Eq, Enum, Bounded)

B.suffixLenses ''AppEnv

_ModalWhyDepends :: Traversal' (Modal s) (B.GenericList Widgets Seq (NonEmpty Path))
_ModalWhyDepends f m = case m of
  ModalWhyDepends l -> ModalWhyDepends <$> f l
  _ -> pure m

compareBySortOrder :: SortOrder -> Path -> Path -> Ordering
compareBySortOrder SortOrderAlphabetical = compare `on` T.toLower . storeNameToShortText . spName
compareBySortOrder SortOrderClosureSize = compare `on` Down . psTotalSize . spPayload
compareBySortOrder SortOrderAddedSize = compare `on` Down . psAddedSize . spPayload

attrTerminal, attrUnderlined :: B.AttrName
attrTerminal = B.attrName "terminal"
attrUnderlined = B.attrName "underlined"

run :: StoreEnv PathStats -> IO ()
run env = do
  -- Create the inverted index, and start evaluating it in the background
  let ii = iiFromList . toList . fmap (\sp -> (storeNameToText (spName sp), sp)) $ seAll env
  _ <- forkIO $ evaluateNF_ ii

  -- Initial state
  let getTime = Clock.getTime Clock.Monotonic
  currTime <- getTime
  let defaultSortOrder = SortOrderClosureSize

  let appEnv =
        AppEnv
          { aeActualStoreEnv =
              env,
            aeInvertedIndex =
              ii,
            aePrevPane =
              B.list WidgetPrevPane S.empty 0,
            aeCurrPane =
              B.list
                WidgetCurrPane
                (S.fromList . sortBy (compareBySortOrder defaultSortOrder) . NE.toList $ seGetRoots env)
                0,
            aeNextPane =
              B.list WidgetNextPane S.empty 0,
            aeParents =
              [],
            aeOpenModal =
              Nothing,
            aeSortOrder =
              defaultSortOrder,
            aeSortOrderLastChanged =
              Clock.TimeSpec 0 0,
            aeCurrTime =
              currTime
          }
          & repopulateNextPane

  -- Create a channel that's fed by current time
  chan <- B.newBChan 10
  void . forkIO $
    forever $ do
      threadDelay (100 * 1000)
      t <- getTime
      _ <- B.writeBChanNonBlocking chan (EventTick t)
      return ()

  -- And run the application
  (_, vty) <- B.customMainWithDefaultVty (Just chan) app appEnv
  V.shutdown vty

  return ()

renderList ::
  Maybe SortOrder ->
  Bool ->
  List ->
  B.Widget Widgets
renderList highlightSort =
  B.renderList
    ( \_
       StorePath
         { spName,
           spPayload = PathStats {psTotalSize, psAddedSize, psDisambiguationChars},
           spRefs,
           spSignatures
         } ->
          let color =
                if null spRefs
                  then B.withAttr attrTerminal
                  else identity
           in color $
                B.hBox
                  [ if null spSignatures
                      then B.txt "  "
                      else B.txt "✓ ",
                    B.txt (storeNameToShortTextWithDisambiguation psDisambiguationChars spName)
                      & underlineWhen SortOrderAlphabetical
                      & B.padRight (B.Pad 1)
                      & B.padRight B.Max,
                    if null spRefs
                      then
                        B.txt (prettySize psTotalSize)
                          & underlineWhen SortOrderClosureSize
                          & underlineWhen SortOrderAddedSize
                      else
                        B.hBox
                          [ B.txt (prettySize psTotalSize)
                              & underlineWhen SortOrderClosureSize,
                            B.txt " (",
                            B.txt (prettySize psAddedSize)
                              & underlineWhen SortOrderAddedSize,
                            B.txt ")"
                          ]
                  ]
    )
  where
    underlineWhen so =
      if Just so == highlightSort
        then B.withDefAttr attrUnderlined
        else identity

app :: B.App (AppEnv s) Event Widgets
app =
  B.App
    { B.appDraw = \env@AppEnv {aeOpenModal} ->
        [ case aeOpenModal of
            Nothing -> B.emptyWidget
            Just (ModalWhyDepends l) -> renderWhyDependsModal l
            Just (ModalSearch l r xs) -> renderSearchModal l r xs
            Just (ModalNotice notice) -> renderNotice notice,
          renderMainScreen env
        ],
      B.appChooseCursor = \_ _ -> Nothing,
      B.appHandleEvent = \e -> do
        s <- get
        case (e, aeOpenModal s) of
          -- main screen
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'q', V.KEsc] ->
                B.halt
          (B.VtyEvent (V.EvKey (V.KChar '?') []), Nothing) ->
            put s {aeOpenModal = Just (ModalNotice helpNotice)}
          (B.VtyEvent (V.EvKey (V.KChar 'w') []), Nothing) -> do
            B.hScrollToBeginning (B.viewportScroll WidgetWhyDependsViewport)
            modify showWhyDepends
          (B.VtyEvent (V.EvKey (V.KChar '/') []), Nothing) ->
            modify $ showAndUpdateSearch "" ""
          (B.VtyEvent (V.EvKey (V.KChar 'y') []), Nothing) -> do
            liftIO (yankToClipboard $ spName (selectedPath s))
              >>= \case
                Right () -> return ()
                Left n -> put s {aeOpenModal = Just (ModalNotice n)}
          (B.VtyEvent (V.EvKey (V.KChar 's') []), Nothing) ->
            put $
              s
                { aeSortOrder = succCycle (aeSortOrder s),
                  aeSortOrderLastChanged = aeCurrTime s
                }
                & sortPanes
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'h', V.KLeft] ->
                modify moveLeft
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'j', V.KDown, V.KChar '\t'] ->
                move B.listMoveDown
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'k', V.KUp, V.KBackTab] ->
                move B.listMoveUp
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'l', V.KRight] ->
                modify moveRight
          (B.VtyEvent (V.EvKey V.KPageUp []), Nothing) ->
            moveF B.listMovePageUp
          (B.VtyEvent (V.EvKey V.KPageDown []), Nothing) ->
            moveF B.listMovePageDown
          -- why-depends modal
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends _))
            | k `elem` [V.KChar 'q', V.KEsc] ->
                put s {aeOpenModal = Nothing}
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends _))
            | k `elem` [V.KChar 'h', V.KLeft] ->
                B.hScrollBy (B.viewportScroll WidgetWhyDependsViewport) (-1)
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends l))
            | k `elem` [V.KChar 'j', V.KDown, V.KChar '\t'] ->
                put s {aeOpenModal = Just $ ModalWhyDepends (B.listMoveDown l)}
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends l))
            | k `elem` [V.KChar 'k', V.KUp, V.KBackTab] ->
                put s {aeOpenModal = Just $ ModalWhyDepends (B.listMoveUp l)}
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends _))
            | k `elem` [V.KChar 'l', V.KRight] ->
                B.hScrollBy (B.viewportScroll WidgetWhyDependsViewport) 1
          (B.VtyEvent (V.EvKey V.KPageUp []), Just (ModalWhyDepends _)) ->
            B.zoom (aeOpenModalL . _Just . _ModalWhyDepends) B.listMovePageUp
          (B.VtyEvent (V.EvKey V.KPageDown []), Just (ModalWhyDepends _)) ->
            B.zoom (aeOpenModalL . _Just . _ModalWhyDepends) B.listMovePageDown
          (B.VtyEvent (V.EvKey V.KEnter []), Just (ModalWhyDepends l)) ->
            let closed = s {aeOpenModal = Nothing}
             in case B.listSelectedElement l of
                  Nothing -> put closed
                  Just (_, path) -> put $ selectPath path closed
          -- search modal
          (B.VtyEvent (V.EvKey V.KEsc []), Just (ModalSearch {})) ->
            put s {aeOpenModal = Nothing}
          (B.VtyEvent (V.EvKey k []), Just (ModalSearch l r xs))
            | k `elem` [V.KDown, V.KChar '\t'] ->
                put s {aeOpenModal = Just $ ModalSearch l r (B.listMoveDown xs)}
          (B.VtyEvent (V.EvKey k []), Just (ModalSearch l r xs))
            | k `elem` [V.KUp, V.KBackTab] ->
                put s {aeOpenModal = Just $ ModalSearch l r (B.listMoveUp xs)}
          (B.VtyEvent (V.EvKey V.KLeft []), Just (ModalSearch l r xs)) ->
            put
              s
                { aeOpenModal =
                    Just $ ModalSearch (T.dropEnd 1 l) (T.takeEnd 1 l <> r) (B.listMoveUp xs)
                }
          (B.VtyEvent (V.EvKey V.KRight []), Just (ModalSearch l r xs)) ->
            put
              s
                { aeOpenModal =
                    Just $ ModalSearch (l <> T.take 1 r) (T.drop 1 r) (B.listMoveUp xs)
                }
          (B.VtyEvent (V.EvKey (V.KChar c) []), Just (ModalSearch l r _))
            | c `Set.member` allowedSearchChars ->
                modify (showAndUpdateSearch (l <> T.singleton c) r)
          (B.VtyEvent (V.EvKey V.KBS []), Just (ModalSearch l r _)) ->
            modify (showAndUpdateSearch (T.dropEnd 1 l) r)
          (B.VtyEvent (V.EvKey V.KEnter []), Just (ModalSearch _ _ xs)) ->
            let closed = s {aeOpenModal = Nothing}
             in case B.listSelectedElement xs of
                  Nothing -> put closed
                  Just (_, path) ->
                    put $
                      selectPath
                        (shortestPathTo (aeActualStoreEnv s) (spName path))
                        closed
          -- notices
          (B.VtyEvent (V.EvKey k []), Just (ModalNotice _))
            | k `elem` [V.KChar 'q', V.KEsc] ->
                put s {aeOpenModal = Nothing}
          -- handle our events
          (B.AppEvent (EventTick t), _) ->
            let new = s {aeCurrTime = t}
             in do
                  put new
                  unless (timePassedSinceSortOrderChange new <= sum (replicate 2 sortOrderChangeHighlightPeriod)) B.continueWithoutRedraw
          -- ignore otherwise
          _ ->
            return (),
      B.appStartEvent = return (),
      B.appAttrMap = \_ ->
        B.attrMap
          V.defAttr
          [ (B.listSelectedFocusedAttr, V.currentAttr `V.withStyle` V.reverseVideo),
            (attrTerminal, B.fg V.blue),
            (attrUnderlined, V.currentAttr `V.withStyle` V.underline)
          ]
    }
  where
    allowedSearchChars :: Set Char
    allowedSearchChars =
      Set.fromList
        ( mconcat
            [ ['a' .. 'z'],
              ['A' .. 'Z'],
              ['0' .. '9'],
              "+-.=?_"
            ]
        )

yankToClipboard :: StoreName -> IO (Either Notice ())
yankToClipboard p =
  Clipboard.copy (toText $ storeNameToPath p)
    <&> \case
      Right () -> Right ()
      Left errs ->
        Left $
          Notice
            "Error"
            ( T.intercalate "\n" $
                "Cannot copy to clipboard: "
                  : map ("  " <>) errs
                  ++ ["Please report this as a bug."]
            )

timePassedSinceSortOrderChange :: AppEnv s -> Clock.TimeSpec
timePassedSinceSortOrderChange env = Clock.diffTimeSpec (aeCurrTime env) (aeSortOrderLastChanged env)

renderMainScreen :: AppEnv s -> B.Widget Widgets
renderMainScreen env@AppEnv {aePrevPane, aeCurrPane, aeNextPane} =
  (B.joinBorders . B.border)
    ( B.hBox
        [ renderList Nothing True aePrevPane,
          B.vBorder,
          renderList shouldHighlightSortOrder True aeCurrPane,
          B.vBorder,
          renderList Nothing False aeNextPane
        ]
    )
    B.<=> renderInfoPane env
  where
    shouldHighlightSortOrder =
      if timePassedSinceSortOrderChange env < sortOrderChangeHighlightPeriod
        then Just (aeSortOrder env)
        else Nothing

renderInfoPane :: AppEnv s -> B.Widget Widgets
renderInfoPane env =
  let selected = selectedPath env
      immediateParents = psImmediateParents $ spPayload selected
      signatures = spSignatures selected
   in B.vBox
        [ let (f, s) = storeNameToSplitShortText (spName selected)
           in B.txt f B.<+> underlineWhen SortOrderAlphabetical (B.txt s),
          [ B.txt $ "NAR Size: " <> prettySize (spSize selected),
            underlineWhen SortOrderClosureSize . B.txt $ "Closure Size: " <> prettySize (psTotalSize $ spPayload selected),
            underlineWhen SortOrderAddedSize . B.txt $ "Added Size: " <> prettySize (psAddedSize $ spPayload selected)
          ]
            & intersperse (B.txt " | ")
            & B.hBox,
          B.txt $
            "Signatures: "
              <> if null signatures
                then "✗"
                else
                  ( signatures
                      & map npsKeyName
                      & T.intercalate ", "
                  ),
          B.txt $
            if null immediateParents
              then "Immediate Parents: -"
              else
                "Immediate Parents ("
                  <> T.pack (show $ length immediateParents)
                  <> "): "
                  <> T.intercalate ", " (map storeNameToShortText immediateParents)
        ]
  where
    underlineWhen so =
      if so == aeSortOrder env
        then B.withAttr attrUnderlined
        else identity

renderModal :: Text -> B.Widget a -> B.Widget a
renderModal title widget =
  widget
    & B.borderWithLabel (B.txt title)
    & B.hLimitPercent 90
    & B.vLimitPercent 60
    & B.centerLayer

helpText :: Text
helpText =
  T.intercalate
    "\n"
    [ "hjkl/Arrow Keys : Navigate",
      "w               : Open why-depends modal",
      "/               : Open search modal",
      "s               : Change sort order",
      "y               : Yank selected path to clipboard",
      "?               : Show help",
      "q/Esc           : Quit / close modal"
    ]

helpNotice :: Notice
helpNotice = Notice "Help" helpText

renderNotice :: Notice -> B.Widget a
renderNotice (Notice title txt) = renderModal title (B.txt txt)

renderWhyDependsModal ::
  B.GenericList Widgets Seq (NonEmpty Path) ->
  B.Widget Widgets
renderWhyDependsModal l =
  B.renderList renderDepends True l
    & B.hLimitPercent 100 -- This limit seems pointless, but otherwise render list takes infinite
    -- amount of horizontal space and 'viewport' below complains.
    & B.viewport WidgetWhyDependsViewport B.Horizontal
    & renderModal "why-depends"
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
                  (fromMaybe 0 $ ((==) `on` fmap spName) route `S.findIndexL` xs)
    }

renderSearchModal :: Text -> Text -> B.GenericList Widgets Seq Path -> B.Widget Widgets
renderSearchModal left right l =
  renderModal "Search" window
  where
    window =
      B.txt left
        B.<+> B.txt "|"
        B.<+> B.txt right
        B.<=> B.hBorder
        B.<=> renderList Nothing True l

showAndUpdateSearch :: Text -> Text -> AppEnv s -> AppEnv s
showAndUpdateSearch left right env@AppEnv {aeInvertedIndex} =
  env {aeOpenModal = Just $ ModalSearch left right results}
  where
    results =
      let xs =
            iiSearch (left <> right) aeInvertedIndex
              & Map.elems
              & S.fromList
       in B.list WidgetSearch xs 1

move :: (List -> List) -> B.EventM n (AppEnv s) ()
move = moveF . modify

moveF :: B.EventM n List () -> B.EventM n (AppEnv s) ()
moveF f = do
  B.zoom aeCurrPaneL f
  modify repopulateNextPane

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
repopulateNextPane env@AppEnv {aeActualStoreEnv, aeNextPane, aeSortOrder} =
  let ref = selectedPath env
   in env
        { aeNextPane =
            B.listReplace
              ( S.sortBy (compareBySortOrder aeSortOrder)
                  . S.fromList
                  . map (seLookup aeActualStoreEnv)
                  $ spRefs ref
              )
              (Just 0)
              aeNextPane
        }

sortPane :: SortOrder -> List -> List
sortPane so l =
  let selected = B.listSelectedElement l
      elems =
        B.listElements l
          & S.sortBy (compareBySortOrder so)
      name = B.getName l
   in mkList so name elems (snd <$> selected)

sortPanes :: AppEnv s -> AppEnv s
sortPanes env@AppEnv {aeParents, aePrevPane, aeCurrPane, aeNextPane, aeSortOrder} =
  env
    { aeCurrPane = sortPane aeSortOrder aeCurrPane,
      aeNextPane = sortPane aeSortOrder aeNextPane,
      aeParents = sortPane aeSortOrder <$> aeParents,
      aePrevPane = sortPane aeSortOrder aePrevPane
    }

selectedPath :: AppEnv s -> Path
selectedPath = NE.head . selectedPaths

selectedPaths :: AppEnv s -> NonEmpty Path
selectedPaths AppEnv {aePrevPane, aeCurrPane, aeParents} =
  let parents =
        mapMaybe
          (fmap snd . B.listSelectedElement)
          (aePrevPane : aeParents)
   in case B.listSelectedElement aeCurrPane of
        Nothing -> error "invariant violation: no selected element"
        Just (_, p) -> p :| parents

selectPath :: NonEmpty Path -> AppEnv s -> AppEnv s
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
          & fmap (\(possible, selected) -> mkList (aeSortOrder env) WidgetPrevPane (S.fromList possible) (Just selected))
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
    emptyPane =
      B.list WidgetPrevPane S.empty 0

mkList ::
  SortOrder ->
  n ->
  Seq Path ->
  Maybe Path ->
  B.GenericList n Seq Path
mkList sortOrder name possible selected =
  let contents = S.sortBy (compareBySortOrder sortOrder) possible
   in B.list name contents 1
        & B.listMoveTo
          (fromMaybe 0 $ selected >>= \s -> ((==) `on` spName) s `S.findIndexL` contents)

-- Utils

prettySize :: Int -> T.Text
prettySize size = case HRF.convertSize $ fromIntegral size of
  HRF.Bytes d -> T.pack (show d)
  HRF.KiB d -> T.pack (show d) <> " KiB"
  HRF.MiB d -> T.pack (show d) <> " MiB"
  HRF.GiB d -> T.pack (show d) <> " GiB"
  HRF.TiB d -> T.pack (show d) <> " TiB"
