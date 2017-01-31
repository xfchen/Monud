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
--import Data.String.Conversions (cs)
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as C8
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


main :: IO ()
main = do

     (hostname:port:_) <- getArgs
     
     handle <- connectMud hostname port
     chan <- newBChan 10
     writeBChan chan $ ServerOutput "Connecting, please wait\n"
     let initialState = UIState
          (E.editorText Input (txt . last) (Just 1) "")
          ""
          []
          ""
          handle

     cfg <- V.standardIOConfig
 
     forkIO $ forever $ runEffect $ (readMudLine handle) >-> (displayLine chan) -- pipe version making manipulating output easier
--         output <- TIO.hGetLine handle 
          
--         when (not $ Data.Text.null output)  
--              $  writeBChan chan (ServerOutput $ output <> "\n")

                              
       
     void $ M.customMain (V.mkVty cfg) (Just chan) app initialState

