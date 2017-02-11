{-# LANGUAGE OverloadedStrings #-}


module AnsiParse
  where

import Pipes
--import qualified Pipes.Prelude as P
--import Pipes.Attoparsec
import qualified Data.Attoparsec.Text as A
import Data.Text (Text, pack)
import Data.Monoid ((<>))
--import Data.Char (isLetter)

import Control.Applicative
import Control.Monad



carriageReturn :: A.Parser Text -- replaces "\r" by "\n" so a variety of mud protocols can be dealt with
carriageReturn = do
  "\10" <|> "\13" <|> "\13\10"
  return $ "\10"

ansiCon :: A.Parser Text -- delete all the ansi control sequences
ansiCon = do
  "\27" >> A.takeWhile (/='m') >> "m"
  return $ ""

deAnsiText :: A.Parser Text
deAnsiText =  A.takeWhile1 (\x -> x /='\27' && x /= '\13') -- no need to worry "\n", just send it through

parseOutPut :: A.Parser Text
parseOutPut =  liftM (foldl (<>) $ pack "" ) $ A.many1 (ansiCon <|> deAnsiText <|> carriageReturn)


rmAnsi :: Pipe Text Text IO ()
rmAnsi = do
  x <- await

  let y = A.parseOnly parseOutPut $ x

  case y of
    Left  e -> yield $ pack e
    Right z -> yield z
