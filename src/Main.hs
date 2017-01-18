import Data.Bits

import Data.Maybe
import Data.List
import Control.Monad
import Control.Exception
import System.IO
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH



import MudData
import Net
import UI


 

main :: IO ()
main = 
   do h <- connectMud hostname port 
      outPut <- hGetContents h
      runCurses outPut `finally` CursesH.end -- Must end curses session even exceptions are raised
   where runCurses outPut =
           do CursesH.start
              cstyles <- CursesH.convertStyles styles
              Curses.cursSet Curses.CursorVisible
              runMud cstyles outPut mudMain 
