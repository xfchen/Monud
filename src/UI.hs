{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Control.Applicative

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import Data.String.Conversions (cs)



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
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , txt
  )

import MudData



-- very simple UI layout
-- a top viewport for output and
-- a one row editor for input

data Name= Output -- Viewport to display the output
         |Input   -- Edit (just line) to input commands
         deriving (Ord, Show, Eq)

drawUi :: MudState -> [T.Widget Name]
drawUi st = [ui]
    where 
        ui = C.center $ B.border $ -- hLimit 80 $ vLimit 24 $
             vBox [ top , B.hBorder , bottom ]
        top =  viewport Output Vertical $ txt $ st^.output
        bottom =  E.renderEditor True (E.editorText Input (txt . last) (Just 1) (st^.command))


outputScroll :: M.ViewportScroll Name
outputScroll = M.viewportScroll Output



-- appEvent needs major work 
-- 1. Kchar events would update the input window
-- 2. add a custom event to handle output received from the host
appEvent :: MudState -> T.BrickEvent Name e -> T.EventM Name (T.Next MudState)
appEvent st (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy outputScroll 1 >> M.continue st
appEvent st (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy outputScroll (-1) >> M.continue st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st 
appEvent st _ = M.continue st

app :: M.App MudState e Name
app =
    M.App { M.appDraw = drawUi 
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }

