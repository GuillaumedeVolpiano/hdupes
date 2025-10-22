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
import Options.Applicative (Parser, help, metavar, short, long, strOption, execParser, info, (<**>), helper, fullDesc, progDesc)
import Data.List.Split (splitOn)

data Args = Args {
  sourceDir :: FilePath,
  dupesDir :: FilePath,
  extensions :: [String]
                 }

app :: App AppState e String
app =  App { appDraw = drawUI
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleEvent
            , appStartEvent =  pure ()
            , appAttrMap = uiAttrMap
           }

args :: Parser Args
args = Args <$>
  strOption (long "from" <> short 'f' <> metavar "From" <> help "Path to the directory with the source files")
  <*> strOption (long "dupes" <> short 'd' <> metavar "Dupes" <> help "Path to the directory with the dupes files")
  <*> (splitOn "," <$> strOption (long "ext" <> short 'e' <> metavar "Extensions" <> help "Comma separated list of extensions to consider"))


main ::  IO ()
main = do
  (Args from to exts) <- execParser . info (args <**> helper) $ (fullDesc <> progDesc "A tool to replace duplicate files with hardlinks, inspired by jdupes")
  (fromDicts, toDicts) <- runEff . runUnix $ do
    fd <- buildDicts from exts
    td <- buildDicts to exts
    pure (fd, td)
  vty <- mkVty defaultConfig
  let (linked, dupes, uncertain, noMatch) = compareDicts fromDicts toDicts
      appState = initialState (S.toList dupes) (S.size linked) (S.size dupes) (S.size uncertain) (S.size noMatch)
  void $ customMain vty (mkVty defaultConfig) Nothing app appState
