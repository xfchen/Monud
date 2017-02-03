{-# LANGUAGE OverloadedStrings #-}


import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import System.IO

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V 
--import Data.String.Conversions (cs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding
import qualified Data.Text.IO as TIO

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





import MudIO
import UI
import Host

--initialState :: UIState

main :: IO ()
main = do

     handle <- connectMud hostname port
     file <- openFile "log.txt" WriteMode
{-
     chan <- newBChan 10
     writeBChan chan $ ServerOutput "Connecting, please wait\n"
     let initialState = UIState
          (E.editorText Input (txt . last) (Just 1) "")
          ""
          []
          ""
          handle

     cfg <- V.standardIOConfig
 
     forkIO $ forever $ do
         output <- BS.hGetContents handle 
         
         when (not $ BS.null output) $ do 
                writeBChan chan (ServerOutput $ output <> "\n")
                TIO.hPutStr file $ decodeUtf8 output

                              
       
     void $ M.customMain (V.mkVty cfg) (Just chan) app initialState
-}
     C8.putStrLn "Testing"

     output <- BS.hGetContents handle
     BS.putStr output
