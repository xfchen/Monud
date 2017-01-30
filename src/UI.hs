{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Trans

import System.IO

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
--import Data.String.Conversions (cs)
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import Data.Text.IO as TIO
--import Data.Text.Encoding
import Data.Text.Zipper

import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V


import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E




import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Widgets.Core
  ( 
    viewport
  , txt
  , vBox
  , visible
  , hLimit
  )



import MudIO 
import Host
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
             , _handle  :: Handle
             }


makeLenses ''UIState
          



drawUi :: UIState -> [T.Widget Name]
drawUi st = [ui]
    where 
        ui = C.center $ --B.border $  hLimit 80 $ -- $ vLimit 24 $
             vBox [ top , B.hBorder , bottom ]
        top =  viewport Output Vertical $ txt $ st^.output
        bottom =  E.renderEditor True $ st^.cli --(E.editorText Input (txt . last) (Just 1) (st^.cmd))


outputScroll :: M.ViewportScroll Name
outputScroll = M.viewportScroll Output


data CustomEvent = ServerOutput Text


appEvent :: UIState -> T.BrickEvent Name CustomEvent -> T.EventM Name (T.Next UIState)
appEvent st ev = 
    case ev of
         T.VtyEvent (V.EvKey V.KEnter [])  -- Enter key pressed, here we need to perform a few things at once
                                           -- 1. update st^.cmd with editor content
                                           -- 2. send the command to the server (some IO)
                                           -- 3. clear the editor
                                           -- 4. continue the application
             -> do
                    let current= head $ E.getEditContents (st^.cli)
                    liftIO $ TIO.hPutStrLn (st ^. handle) $ current 
                    M.continue (st & cmd .~ current & history %~ ( ++ [current] ) & output %~ (<> current) & cli %~ E.applyEdit clearZipper)
         T.VtyEvent (V.EvKey V.KEsc [])    -- Esc pressed, quit the program
             ->  M.halt st
         T.VtyEvent x                    -- Let the default editor event handler take care of this 
             -> T.handleEventLensed st cli E.handleEditorEvent x >>= M.continue 
         T.AppEvent (ServerOutput t)     -- To handle custome evenets; i.e. when outpus is received from server
                                          -- This is a tricky function since it does several things at once; 
                                          -- It updates the UIState with the output send through the BChannel
                                          -- and then scrolls the viewport before the application continues
             -> M.vScrollToEnd outputScroll>> M.continue (st & output %~ (<> t))
         _   -> M.continue st


app :: M.App UIState CustomEvent Name
app =
    M.App { M.appDraw = drawUi 
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.showFirstCursor
          }
