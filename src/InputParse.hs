{-# LANGUAGE OverloadedStrings #-}


module InputParse
  where

import Pipes

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as C
import Data.Text ( Text
                 , cons
                 , pack
                 , unpack
                 )


import Data.Monoid ((<>))


import Control.Applicative
import Control.Monad


data InputVal = Cmd Text
              | Script Text
              | NumberedCmd (Int, Text)
              | NumberedScript (Int, Text)

showVal :: InputVal -> String
showVal (Cmd c) = unpack c
showVal (Script s) = unpack s
showVal (NumberedCmd (times,c)) = show times ++ " x " ++ unpack c
showVal (NumberedScript (times,s)) = show times ++ " x " ++ unpack s

instance Show InputVal where show = showVal



parseNumberedScript :: A.Parser InputVal
parseNumberedScript = do
  A.skipSpace
  times <- A.decimal
  A.skipSpace
  A.char '#'
  A.skipSpace
  script <- A.takeWhile (/=';')
  C.option "" ";"
  return $ NumberedScript (times, script)

parseNumberedCmd :: A.Parser InputVal
parseNumberedCmd = do
  A.skipSpace
  times <- A.decimal
  A.skipSpace
  first <- A.letter
  rest <- A.takeWhile (/=';')
  C.option "" ";"
  let cmd = cons first rest
  return $ NumberedCmd (times, cmd)


parseScript :: A.Parser InputVal
parseScript = do
  A.skipSpace
  A.char '#'
  A.skipSpace
  script <- A.takeWhile (/=';')
  C.option "" ";"
  return $ Script script

parseCmd :: A.Parser InputVal
parseCmd = do
  A.skipSpace
  first <- A.letter
  rest <- A.takeWhile (/=';')
  C.option "" ";"
  let cmd = cons first rest
  return $ Cmd cmd

parseEmpty :: A.Parser InputVal
parseEmpty = do
  A.endOfLine
  return $ Cmd "\r"

parseExpr :: A.Parser InputVal
parseExpr =  parseEmpty  <|> parseNumberedScript <|> parseNumberedCmd <|> parseScript <|> parseCmd


parseInput :: A.Parser [InputVal]
parseInput =  C.many1 parseExpr
