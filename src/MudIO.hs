{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}


module MudIO where



import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.BSD
import System.IO hiding (hGetLine)
import Data.Text  (Text)
import Data.Text.IO (hGetLine)
--import Control.Monad (unless)
--
import Data.Monoid ((<>))

import Brick.BChan

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Attoparsec
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)




connectMud :: HostName -- ^ Remote hostname, or localhost
           -> String -- ^ Port number of name; 5555 is default
           -> IO Handle  -- ^ Handle to use for mud

connectMud hostname port = do
          addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
          let serveraddr = head addrinfos
          sock <- socket (addrFamily serveraddr) Stream defaultProtocol
          setSocketOption sock KeepAlive 1
          connect sock (addrAddress serveraddr)
          h <- socketToHandle sock ReadWriteMode
          hSetBuffering h (NoBuffering)
          return  h

-- end of function connectMud

readMudLine:: Handle -> Producer Text IO ()
readMudLine h =
     (liftIO $ hGetLine h) >>= yield



data CustomEvent = ServerOutput Text

displayLine :: BChan CustomEvent -> Consumer Text IO ()
displayLine c = do
     t <- await
     liftIO $ writeBChan c (ServerOutput  $ t <> "\n")
