{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where


import Prelude hiding (log)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Trans

import System.IO

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import Data.Text ( Text
                 , pack
                 )
import Data.Text.IO as TIO

import Data.Text.Zipper

import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V


--import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Types as T
import Brick.AttrMap
  ( attrMap
  )
import Brick.Widgets.Core
  (
    viewport
  , txt
  , vBox
  , hBox
  , visible
  , hLimit
  )


import  Data.Attoparsec.Text (parseOnly)


import MudIO
import InputParse


-- very simple UI layout
-- a top viewport for output and
-- a one row editor for input

data Name= Output -- Viewport to display the output
         |Input   -- Edit (just line) to input commands
         deriving (Ord, Show, Eq)


data UIState =
  UIState { _cli     :: E.Editor Text Name     -- Editor widget to input commands
          , _output  :: Text                   -- Output received from the host
          , _history :: [Text]                 -- History of commands inputed by user
          , _cmd     :: Text                   -- Current command, possibly incomplete
          , _handle  :: Handle                 -- handle to the server socket
          , _log     :: Handle                 -- handle to the log file
          }


makeLenses ''UIState


drawUi :: UIState -> [T.Widget Name]
drawUi st = [ui]
  where
    ui = C.center $ B.border $ -- hLimit 80 $ -- $ vLimit 24 $
         vBox [ top , B.hBorder , bottom ]
    top =  viewport Output T.Vertical $  txt $ st^.output
    bottom =  E.renderEditor True $ st^.cli --(E.editorText Input (txt . last) (Just 1) (st^.cmd))


outputScroll :: M.ViewportScroll Name
outputScroll = M.viewportScroll Output





appEvent :: UIState -> T.BrickEvent Name CustomEvent -> T.EventM Name (T.Next UIState)
appEvent st ev =
  case ev of
    T.VtyEvent (V.EvKey V.KEnter []) -> do
      let input = head $ E.getEditContents (st^.cli)
      let parsed = parseOnly parseInput $ input
      case parsed of
        Left _ -> do -- | input is incorrect
          liftIO $ TIO.hPutStrLn  (st^.handle) input
          M.vScrollToEnd outputScroll
          M.continue (st & output %~ (<>  input <> "\n") & cli %~ E.applyEdit clearZipper)

        Right inputVals -> do -- |inputvals has type [InputVal] as defined in InputParse.hs
          forM_  inputVals runInput
          M.vScrollToEnd outputScroll
          M.continue  ( st  &  output %~ ( <>  (pack . show $ inputVals)) & cli  %~ E.applyEdit clearZipper)
            where
              runInput input =
                    case input of
                      Cmd current ->
                        liftIO $ TIO.hPutStrLn (st^.handle) $ current
                      Script script ->
                        liftIO $ TIO.hPutStrLn (st^.handle) $ script
                      NumberedCmd (times, current) ->
                        liftIO $ TIO.hPutStrLn (st^.handle) $ foldl (<>) (pack "") $ replicate times $ current <> "\n"
                      NumberedScript (times, script) ->
                        liftIO $ TIO.hPutStrLn (st^.handle) $ foldl (<>) (pack "") $ replicate times $ script <> "\n"




    T.VtyEvent (V.EvKey V.KEsc [])    -- Esc pressed, quit the program
      ->  M.halt st
    T.VtyEvent (V.EvKey (V.KFun 12) [])  -> do  -- F12 pressed, write to log file
      liftIO $ TIO.hPutStrLn (st^.log) $ (st^.output)
      M.continue (st & output .~ "")

    T.VtyEvent x                    -- Let the default editor event handler take care of this
      -> T.handleEventLensed st cli E.handleEditorEvent x >>= M.continue
    T.AppEvent (ServerOutput t)     -- To handle custome evenets; i.e. when outpus is received from server
      -- This is a tricky function since it does several things at once;
                                          -- It updates the UIState with the output send through the BChannel
                                          -- and then scrolls the viewport before the application continues
      -> M.vScrollToEnd outputScroll  >>  M.continue (st & output %~ ( <> t))
    _   -> M.continue st


app :: M.App UIState CustomEvent Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.showFirstCursor
          }
