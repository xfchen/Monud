{-# LANGUAGE OverloadedStrings #-}



--------------------------------------------------
import Control.Applicative
import Control.Monad
--import Control.Monad.Trans
import Control.Concurrent

import System.IO

import Options.Applicative

import Control.Monad (void)

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
import InputParse

data Options = Options
  { hostname    ::  String
  , port :: String
  , file :: String }

parseOptions :: Parser Options
parseOptions =  Options
        <$> strOption
            ( long "hostname"
            <> short 'h'
            <> metavar "HOSTNAME"
            <> help "ip/FQDN of the Mud server" )
        <*> strOption
            ( long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "port number the Mud server listens on")
        <*> strOption
            ( long "log"
            <> short 'l'
            <> metavar "LOGFILE"
            <> help "file to log the session")


main :: IO ()
main = session =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      (  fullDesc
      <> progDesc "Monud -- A Mud client written in Haskell"
      <> header "Thank you for using Monud")

session :: Options -> IO ()
session(Options hostname port log) = do

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
