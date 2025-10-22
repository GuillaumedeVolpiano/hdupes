{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Brick                      (App (..), customMain)
import           Brick.Main                 (neverShowCursor)
import           Data.Functor               (void)
import qualified Data.Set                   as S (size, toList)
import           Effectful                  (runEff)
import           Effectful.Unix             (runUnix)
import           Graphics.Vty               (defaultConfig)
import           Graphics.Vty.Platform.Unix (mkVty)
import           UI                         (AppState, drawUI, handleEvent,
                                             initialState, uiAttrMap)
import Utils (buildDicts, compareDicts)

from :: FilePath
from = "/opt/movies/downloads"

to :: FilePath
to = "/opt/movies/finished"

exts :: [String]
exts = ["mkv","mp4","avi"]

app :: App AppState e String
app =  App { appDraw = drawUI
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleEvent
            , appStartEvent =  pure ()
            , appAttrMap = uiAttrMap
           }



main :: IO ()
main = do
  (fromDicts, toDicts) <- runEff . runUnix $ do
    fd <- buildDicts from exts
    td <- buildDicts to exts
    pure (fd, td)
  vty <- mkVty defaultConfig
  let (linked, dupes, uncertain, noMatch) = compareDicts fromDicts toDicts
      appState = initialState (S.toList dupes) (S.size linked) (S.size dupes) (S.size uncertain) (S.size noMatch)
  void $ customMain vty (mkVty defaultConfig) Nothing app appState
