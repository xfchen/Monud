{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}


module MudIO where



import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import System.IO
--import qualified Data.ByteString as BS
--import qualified Data.Text       as T


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
