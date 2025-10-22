{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module UI (
            AppState
          , initialState
          , drawUI
          , handleEvent
          , uiAttrMap
  )

where

import Brick.Types (Widget)
import Brick.Widgets.Core (str, hBox)
import Brick (BrickEvent(..), EventM, halt, modify, gets, vBox, ViewportType (Vertical), viewport, viewportScroll, vScrollBy, hLimitPercent, (<+>), Padding (Max), padRight, AttrMap, attrMap, on, attrName, withAttr, AttrName, vLimitPercent)
import qualified Graphics.Vty as V (reverseVideo, withStyle, Event (EvKey), Key (KEsc, KDown, KUp, KChar, KEnter), defAttr, blue, white)
import Brick.Widgets.Border (border, hBorder)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Utils (replaceDupes)
import Effectful.Unix (runUnix)
import Effectful (runEff)

data AppState = AppState
  { selectedItems :: [(Bool, (FilePath, FilePath))]
  , cursorPos      :: Int
  , linked :: Int
  , identified :: Int
  , uncertain :: Int
  , noMatch :: Int
  }

selectedAttr :: AttrName
selectedAttr = attrName "selected"

uiAttrMap :: AppState -> AttrMap
uiAttrMap _ = attrMap V.defAttr
  [ (selectedAttr, V.white `on` V.blue `V.withStyle` V.reverseVideo)
  ]


viewportName :: String
viewportName = "dupesViewPort"

initialState :: [(FilePath, FilePath)] -> Int -> Int -> Int -> Int -> AppState
initialState substitutions = AppState (map (True,) substitutions) 0

renderSubstitution :: Bool -> (FilePath, FilePath) -> Widget n
renderSubstitution isSelected (to, from) 
  | isSelected = withAttr selectedAttr row
  | otherwise = row
  where
    row = hLimitPercent 45 (padRight Max $ str to) <+> str "   " <+> str from

handleEvent :: BrickEvent String e -> EventM String AppState ()
handleEvent e = case e of
  VtyEvent (V.EvKey V.KEsc []) -> halt
  VtyEvent (V.EvKey V.KDown []) -> do
    pos <- gets cursorPos
    sel <- gets selectedItems
    let newPos = min (pos + 1) (length sel - 1)
    when (newPos > pos) $
      vScrollBy (viewportScroll viewportName) 1
    modify (\s -> s { cursorPos = newPos })
  VtyEvent (V.EvKey V.KUp []) -> do
    pos <- gets cursorPos
    let newPos = max (pos - 1) 0
    when (newPos < pos) $
      vScrollBy (viewportScroll viewportName) (-1)
    modify (\s -> s { cursorPos = newPos })
  VtyEvent (V.EvKey (V.KChar ' ') []) -> do
      pos <- gets cursorPos
      selected <- gets selectedItems
      let (isSelected, item) = selected !! pos
          newItems = take pos selected ++ [(not isSelected, item)] ++ drop (pos + 1) selected
      modify (\s -> s { selectedItems = newItems })
  VtyEvent (V.EvKey V.KEnter []) -> do
    selected <- gets selectedItems
    let finalList = map snd . filter fst $ selected
    liftIO $ runEff . runUnix . replaceDupes $ finalList
    halt
  _ -> pure ()

drawUI :: AppState -> [Widget String]
drawUI s = [border viewportContent, hBorder, statusLine]
  where
      selected = selectedItems s
      pos = cursorPos s
      dedupes = zipWith (\i (isSelected, item) ->
                        renderSubstitution (i == pos && isSelected) item)
                      [0..] selected
      viewportContent = viewport viewportName Vertical . vLimitPercent 90 . vBox $ dedupes
      statusLine = hBox [str ("linked: " ++ show (linked s)), str ("dupes: " ++ show (identified s))
                        , str ("uncertain: " ++ show (uncertain s)), str ("no match: " ++ show (noMatch s))]
