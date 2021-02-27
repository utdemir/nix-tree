module App (run, helpText) where

import qualified Brick as B
import qualified Brick.BChan as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.List as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as V
import InvertedIndex
import PathStats
import qualified System.Clock as Clock
import qualified System.HrfSize as HRF

data Event
  = EventTick Clock.TimeSpec

data Widgets
  = WidgetPrevPane
  | WidgetCurrPane
  | WidgetNextPane
  | WidgetWhyDepends
  | WidgetSearch
  deriving (Show, Eq, Ord)

data Modal s
  = ModalHelp
  | ModalWhyDepends (B.GenericList Widgets Seq (NonEmpty (Path s)))
  | ModalSearch Text Text (B.GenericList Widgets Seq (Path s))

succCycle :: forall a. (Bounded a, Enum a) => a -> a
succCycle a
  | fromEnum a == fromEnum (maxBound @a) = minBound
  | otherwise = succ a

data AppEnv s = AppEnv
  { aeActualStoreEnv :: StoreEnv s (PathStats s),
    aeInvertedIndex :: InvertedIndex,
    aePrevPane :: List s,
    aeCurrPane :: List s,
    aeNextPane :: List s,
    aeParents :: [List s],
    aeOpenModal :: Maybe (Modal s),
    aeSortOrder :: SortOrder,
    aeSortOrderLastChanged :: Clock.TimeSpec,
    aeCurrTime :: Clock.TimeSpec
  }

type Path s = StorePath s (StoreName s) (PathStats s)

type List s = B.GenericList Widgets Seq (Path s)

data SortOrder
  = SortOrderAlphabetical
  | SortOrderClosureSize
  | SortOrderAddedSize
  deriving (Show, Eq, Enum, Bounded)

compareBySortOrder :: SortOrder -> Path s -> Path s -> Ordering
compareBySortOrder SortOrderAlphabetical = compare `on` T.toLower . storeNameToShortText . spName
compareBySortOrder SortOrderClosureSize = compare `on` Down . psTotalSize . spPayload
compareBySortOrder SortOrderAddedSize = compare `on` Down . psAddedSize . spPayload

attrTerminal, attrUnderlined :: B.AttrName
attrTerminal = "terminal"
attrUnderlined = "underlined"

run :: StoreEnv s (PathStats s) -> IO ()
run env = do
  -- Create the inverted index, and start evaluating it in the background
  let ii = iiFromList . toList . map (storeNameToText . spName) $ seAll env
  _ <- forkIO $ evaluate (rnf ii)

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
      threadDelay (100 * 100)
      t <- getTime
      _ <- B.writeBChanNonBlocking chan (EventTick t)
      return ()

  -- And run the application
  let mkVty = V.mkVty V.defaultConfig
  initialVty <- mkVty
  _ <- B.customMain initialVty mkVty (Just chan) app appEnv

  return ()

renderList ::
  Maybe SortOrder ->
  Bool ->
  List s ->
  B.Widget Widgets
renderList highlightSort isFocused =
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
                B.hBox
                  [ B.txt (storeNameToShortText spName)
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
    isFocused
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
            Just ModalHelp -> renderHelpModal
            Just (ModalWhyDepends l) -> renderWhyDependsModal l
            Just (ModalSearch l r xs) -> renderSearchModal l r xs,
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
          (B.VtyEvent (V.EvKey (V.KChar '/') []), Nothing) ->
            B.continue $ showAndUpdateSearch "" "" s
          (B.VtyEvent (V.EvKey (V.KChar 's') []), Nothing) ->
            B.continue $
              s
                { aeSortOrder = succCycle (aeSortOrder s),
                  aeSortOrderLastChanged = aeCurrTime s
                }
                & sortPanes
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'h', V.KLeft] ->
              B.continue $ moveLeft s
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'j', V.KDown, V.KChar '\t'] ->
              B.continue $ move B.listMoveDown s
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'k', V.KUp, V.KBackTab] ->
              B.continue $ move B.listMoveUp s
          (B.VtyEvent (V.EvKey k []), Nothing)
            | k `elem` [V.KChar 'l', V.KRight] ->
              B.continue $ moveRight s
          (B.VtyEvent (V.EvKey V.KPageUp []), Nothing) ->
            B.continue =<< moveF B.listMovePageUp s
          (B.VtyEvent (V.EvKey V.KPageDown []), Nothing) ->
            B.continue =<< moveF B.listMovePageDown s
          -- why-depends modal
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends _))
            | k `elem` [V.KChar 'q', V.KEsc] ->
              B.continue s {aeOpenModal = Nothing}
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends l))
            | k `elem` [V.KChar 'j', V.KDown, V.KChar '\t'] ->
              B.continue s {aeOpenModal = Just $ ModalWhyDepends (B.listMoveDown l)}
          (B.VtyEvent (V.EvKey k []), Just (ModalWhyDepends l))
            | k `elem` [V.KChar 'k', V.KUp, V.KBackTab] ->
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
          -- search modal
          (B.VtyEvent (V.EvKey V.KEsc []), Just (ModalSearch _ _ _)) ->
            B.continue s {aeOpenModal = Nothing}
          (B.VtyEvent (V.EvKey k []), Just (ModalSearch l r xs))
            | k `elem` [V.KDown, V.KChar '\t'] ->
              B.continue s {aeOpenModal = Just $ ModalSearch l r (B.listMoveDown xs)}
          (B.VtyEvent (V.EvKey k []), Just (ModalSearch l r xs))
            | k `elem` [V.KUp, V.KBackTab] ->
              B.continue s {aeOpenModal = Just $ ModalSearch l r (B.listMoveUp xs)}
          (B.VtyEvent (V.EvKey V.KLeft []), Just (ModalSearch l r xs)) ->
            B.continue
              s
                { aeOpenModal =
                    Just $ ModalSearch (T.dropEnd 1 l) (T.takeEnd 1 l <> r) (B.listMoveUp xs)
                }
          (B.VtyEvent (V.EvKey V.KRight []), Just (ModalSearch l r xs)) ->
            B.continue
              s
                { aeOpenModal =
                    Just $ ModalSearch (l <> T.take 1 r) (T.drop 1 r) (B.listMoveUp xs)
                }
          (B.VtyEvent (V.EvKey (V.KChar c) []), Just (ModalSearch l r _))
            | c `Set.member` allowedSearchChars ->
              B.continue (showAndUpdateSearch (l <> T.singleton c) r s)
          (B.VtyEvent (V.EvKey (V.KBS) []), Just (ModalSearch l r _)) ->
            B.continue (showAndUpdateSearch (T.dropEnd 1 l) r s)
          (B.VtyEvent (V.EvKey V.KEnter []), Just (ModalSearch _ _ xs)) ->
            let closed = s {aeOpenModal = Nothing}
             in case B.listSelectedElement xs of
                  Nothing -> B.continue closed
                  Just (_, path) ->
                    B.continue $
                      selectPath
                        (shortestPathTo (aeActualStoreEnv s) (spName path))
                        closed
          -- help modal
          (B.VtyEvent (V.EvKey k []), Just ModalHelp)
            | k `elem` [V.KChar 'q', V.KEsc] ->
              B.continue s {aeOpenModal = Nothing}
          -- handle our events
          (B.AppEvent (EventTick t), Nothing) ->
            B.continue $ s {aeCurrTime = t}
          -- ignore otherwise
          _ ->
            B.continue s,
      B.appStartEvent = \s -> return s,
      B.appAttrMap = \_ ->
        B.attrMap
          (V.white `B.on` V.black)
          [ (B.listSelectedFocusedAttr, V.black `B.on` V.white),
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
      let timePassed = Clock.diffTimeSpec (aeCurrTime env) (aeSortOrderLastChanged env)
       in if timePassed < Clock.TimeSpec 0 (500 * 1_000_000)
            then Just (aeSortOrder env)
            else Nothing

renderInfoPane :: AppEnv s -> B.Widget Widgets
renderInfoPane env =
  let selected = selectedPath env
      immediateParents = psImmediateParents $ spPayload selected
   in B.vBox
        [ ( let (f, s) = storeNameToSplitShortText (spName selected)
             in B.txt f B.<+> underlineWhen SortOrderAlphabetical (B.txt s)
          ),
          [ B.txt $ "NAR Size: " <> prettySize (spSize selected),
            underlineWhen SortOrderClosureSize . B.txt $ "Closure Size: " <> prettySize (psTotalSize $ spPayload selected),
            underlineWhen SortOrderAddedSize . B.txt $ "Added Size: " <> prettySize (psAddedSize $ spPayload selected)
          ]
            & intersperse (B.txt " | ")
            & B.hBox,
          B.txt $
            if null immediateParents
              then "Immediate Parents: -"
              else
                "Immediate Parents (" <> T.pack (show $ length immediateParents) <> "): "
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
      "q/Esc:          : Quit / close modal",
      "w               : Open why-depends mode",
      "/               : Open search mode",
      "s               : Change sort order",
      "?               : Show help"
    ]

renderHelpModal :: B.Widget a
renderHelpModal = renderModal "Help" (B.txt helpText)

renderWhyDependsModal ::
  B.GenericList Widgets Seq (NonEmpty (Path s)) ->
  B.Widget Widgets
renderWhyDependsModal l =
  renderModal "why-depends" (B.renderList renderDepends True l)
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

renderSearchModal :: Text -> Text -> B.GenericList Widgets Seq (Path s) -> B.Widget Widgets
renderSearchModal left right l =
  renderModal "Search" window
  where
    window =
      B.txt left B.<+> B.txt "|" B.<+> B.txt right
        B.<=> B.hBorder
        B.<=> renderList Nothing True l

showAndUpdateSearch :: Text -> Text -> AppEnv s -> AppEnv s
showAndUpdateSearch left right env@AppEnv {aeActualStoreEnv, aeInvertedIndex} =
  env {aeOpenModal = Just $ ModalSearch left right results}
  where
    results =
      let xs =
            iiSearch (left <> right) aeInvertedIndex
              & (S.fromList . Set.toList)
              & fmap (seLookup aeActualStoreEnv . StoreName)
       in B.list WidgetSearch xs 1

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

sortPane :: SortOrder -> List s -> List s
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
  Seq (Path s) ->
  Maybe (Path s) ->
  B.GenericList n Seq (Path s)
mkList sortOrder name possible selected =
  let contents = S.sortBy (compareBySortOrder sortOrder) possible
   in B.list name contents 1
        & B.listMoveTo
          (fromMaybe 0 $ selected >>= \s -> (((==) `on` spName) s) `S.findIndexL` contents)

-- Utils

prettySize :: Int -> T.Text
prettySize size = case HRF.convertSize $ fromIntegral size of
  HRF.Bytes d -> T.pack (show d)
  HRF.KiB d -> T.pack (show d) <> " KiB"
  HRF.MiB d -> T.pack (show d) <> " MiB"
  HRF.GiB d -> T.pack (show d) <> " GiB"
  HRF.TiB d -> T.pack (show d) <> " TiB"
