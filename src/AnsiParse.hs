{-# LANGUAGE OverloadedStrings #-}


module AnsiParse
  where

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Attoparsec
import qualified Data.Attoparsec.Text as A
import Data.Text (Text, pack)

import Control.Applicative

notAnsi :: A.Parser Text
notAnsi = "\27" *> A.takeWhile (/='m') *> "m" *> A.takeWhile (/='\27')

--rmAnsi :: Monad m => Producer Text m r -> Producer Text m (Either (ParsingError, Producer Text m r) r)
--rmAnsi =  parsed notAnsi

rmAnsi :: Pipe Text Text IO ()
rmAnsi = do
  x <- await
  let y = A.parseOnly notAnsi x
  case y of
    Left  e -> yield $ pack e
    Right z -> yield z
