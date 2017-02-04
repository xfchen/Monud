{-# LANGUAGE OverloadedStrings #-}


--------------------------------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import System.IO

import System.Environment

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import Data.Text (Text, null)
import qualified Data.Text.IO as TIO
import Data.Text.Encoding

import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V


import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import Brick.BChan


import Brick.Widgets.Core
  ( txt )

import Pipes

import MudIO
import UI
import AnsiParse

main :: IO ()
main = do

     (hostname:port:log:_) <- getArgs
     file <- openFile log AppendMode
     handle <- connectMud hostname port
     chan <- newBChan 10
     writeBChan chan $ ServerOutput "Connecting, please wait\n"
     let mudState = UIState
          (E.editorText Input (txt . last) (Just 1) "")
          ""
          []
          ""
          handle
          file

     cfg <- V.standardIOConfig

     forkIO $ forever $ runEffect $
       (readMudLine handle) >->  rmAnsi  >-> displayLine chan -- pipe version making manipulating output easier


     void $ M.customMain (V.mkVty cfg) (Just chan) app mudState


     hClose file
