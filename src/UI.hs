{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Control.Applicative
import Control.Monad

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
--import Data.String.Conversions (cs)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding



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
  )

-- very simple UI layout
-- a top viewport for output and
-- a one row editor for input

data Name= Output -- Viewport to display the output
         |Input   -- Edit (just line) to input commands
         deriving (Ord, Show, Eq)


data UIState =
     UIState { --_display :: T.Viewport             -- Viewport widget to display the output
               _cli     :: E.Editor Text Name   -- Editor widget to input commands
             , _output  :: Text                   -- Output received from the host
             , _history :: [Text]                 -- History of commands inputed by user
             , _cmd     :: Text                   -- Current command, possibly incomplete
             }


makeLenses ''UIState
          



drawUi :: UIState -> [T.Widget Name]
drawUi st = [ui]
    where 
        ui = C.center $ B.border $ -- hLimit 80 $ vLimit 24 $
             vBox [ top , B.hBorder , bottom ]
        top =  viewport Output Vertical $ txt $ st^.output
        bottom =  E.renderEditor True $ st^.cli --(E.editorText Input (txt . last) (Just 1) (st^.cmd))


outputScroll :: M.ViewportScroll Name
outputScroll = M.viewportScroll Output

initialState :: UIState
initialState = UIState
     --viewport Output 
     (E.editorText Input (txt . last) (Just 1) "") 
      ""
      []
      ""

data CustomEvent = ServerOutput BS.ByteString

appEvent :: UIState -> T.BrickEvent Name CustomEvent -> T.EventM Name (T.Next UIState)
appEvent st ev = 
    case ev of
         T.VtyEvent (V.EvKey V.KEnter [])  -- Enter key pressed, send the editor content to server
             -> M.continue $ st & cmd .~ (head $ E.getEditContents $ st^.cli)
                  
         T.VtyEvent (V.EvKey V.KEsc [])    -- Esc pressed, quit the program
             -> M.halt st
         T.VtyEvent x                    -- Let the default editor event handler take care of this 
             -> T.handleEventLensed st cli E.handleEditorEvent x >>= M.continue 
         T.AppEvent (ServerOutput bs)     -- To handle custome evenets; i.e. when outpus is received from server
                                          -- This is a tricky function since it does several things at once; 
                                          -- In the first pair of brackets we define a lambda function
                                          -- And in the second pair we update the UIState with the output we receive from the server and pass the new state to the lambda function
             -> (\st -> (M.vScrollToEnd outputScroll >> M.continue st)) (st & output %~ (<> (decodeUtf8 bs)))
         _   -> M.continue st


app :: M.App UIState CustomEvent Name
app =
    M.App { M.appDraw = drawUi 
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }
