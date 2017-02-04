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
ansiCon  = fmap (foldl (<>) $ pack "") $ A.many1 ("\27" >> A.takeWhile (/='m') >> "m")


rmAnsi :: Pipe Text Text IO ()
rmAnsi = do
  x <- await
  let y = fmap (foldl (<>) $ pack "") $ A.parseOnly (A.sepBy (A.takeWhile (not . A.isEndOfLine)) ansiCon) x


  case y of
    Left  e -> yield $ pack e
    Right z -> yield z
