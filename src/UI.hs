{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module UI where


import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V



import Control.Applicative

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
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
  , str
  )

import MudData


data Name= Output -- Window that displays the output
         |Input   -- Window (just the bottom line) to input commands
         deriving (Ord, Show, Eq)

drawUi :: MudState -> [T.Widget Name]
drawUi st = [ui]
    where 
        ui = C.center $ B.border $ -- hLimit 80 $ vLimit 24 $
             vBox [ top , B.hBorder , bottom ]
        top =  viewport Output Vertical $ str $ st^.output
        bottom =  vLimit 1 $ viewport Input Both $ str $ st^.command


outputScroll :: M.ViewportScroll Name
outputScroll = M.viewportScroll Output

inputScroll :: M.ViewportScroll Name
inputScroll = M.viewportScroll Input

appEvent :: MudState -> T.BrickEvent Name e -> T.EventM Name (T.Next MudState)
appEvent st (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy outputScroll 1 >> M.continue st
appEvent st (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy outputScroll (-1) >> M.continue st
appEvent st (T.VtyEvent (V.EvKey V.KDown [V.MCtrl])) = M.vScrollBy inputScroll 1 >> M.continue st
appEvent st (T.VtyEvent (V.EvKey V.KUp [V.MCtrl]))  = M.vScrollBy inputScroll (-1) >> M.continue st 
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

main :: IO ()
main = void $ M.defaultMain app ()
