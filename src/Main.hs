-- file: ch27/syslogtcpclient.hs
import Data.Bits
import Data.Maybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import Data.List
import System.IO
import Control.Monad


import MudData
connectMud :: HostName -- ^ Remote hostname, or localhost
           -> String   -- ^ Port number of name; 5555 is default
           -> IO Handle  -- ^ Handle to use for mud

connectMud hostname port = do
          addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
          let serveraddr = head addrinfos
          sock <- socket (addrFamily serveraddr) Stream defaultProtocol
          setSocketOption sock KeepAlive 1
          connect sock (addrAddress serveraddr)
          h <- socketToHandle sock ReadWriteMode
          hSetBuffering h (BlockBuffering Nothing)
          return  h

-- end of function connectMud

closeMud :: Handle -> IO ()
closeMud h = hClose h

-- end of function closeMud

runMud :: Handle -> IO ()
runMud h = do
          hGetContents h >>= putStr
          runMud h
-- end of function runMud

-- end of funtions getTermHeight, getTermWidth
 

main :: IO ()
main = do 
     h <- connectMud hostname port
     runMud h
