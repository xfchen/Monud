{-# LANGUAGE OverloadedStrings #-}


module AnsiParse
  where

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Attoparsec
import qualified Data.Attoparsec.Text as A
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Data.Char (isLetter)

import Control.Applicative

ansiCon :: A.Parser Text
ansiCon  = "\27" >> A.takeWhile (/='m') >> "m"


--rmAnsi :: Monad m => Producer Text m r -> Producer Text m (Either (ParsingError, Producer Text m r) r)
--rmAnsi =  parsed notAnsi

rmAnsi :: Pipe Text Text IO ()
rmAnsi = do
  x <- await
  let y = fmap (foldl (<>) $ pack "") $ A.parseOnly (A.sepBy (A.takeWhile (/='\27')) ansiCon) x
  case y of
    Left  e -> yield $ pack e
    Right z -> yield z
