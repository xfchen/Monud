{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}


{-| In this module, we will be dealing with Ansi outputs from the server using
  the Megaparsec package.
-}


module AnsiParse where

{-|
import Data.Text 
import Pipes
import Text.Megaparsec
import Text.Megaparsec.Perm
import Text.Megaparsec.Expr
import Text.Megaparsec.Lexer
import Text.Megaparsec.Text
import Control.Applicative
import Control.Monad


The following is the String version for matching an ansi escape sequence like 
  "^[[1m", "^[[1;2;m" or "^[[1;2;3m" 

parseConSeq :: Parser String -- parse to ansi control sequences like "[1;2;3m"
parseConSeq =  ((++) <$> string "\27[" <*> some digitChar) >> string "m"
           <|> ((++) <$> string "\27[" <*> some digitChar) >> string ";" >> (some digitChar) >> string "m"
           <|> ((++) <$> string "\27[" <*> some digitChar) >> string ";" >> (some digitChar) >> string ";" >> (some digitChar) >> string "m"

-}               

import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<|>),
                            (<*),
                            pure)
import Control.Monad
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)


data ConSeq = Esc Text
            | Command Integer
            | Sep Char

parseEsc :: Parser ConSeq 
parseEsc = Esc <$> A.string "\27[" 

parseSep :: Parser ConSeq
parseSep = Sep <$> (A.char ';' <|> A.char 'm')

parseNum :: Parser ConSeq
parseNum = Command <$> A.decimal


parsePkuXkx :: Parser ConSeq
parsePkuXkx  =  parseEsc >> parseNum >> parseSep -- parse to ansi control sequences like "^[[1m"
            <|> parseEsc >> parseNum >> parseSep >> parseNum >> parseSep -- parse to ansi control sequences like "[1;2m"
            <|> parseEsc >> parseNum >> parseSep >> parseNum >> parseSep >> parseNum >> parseSep -- parse to ansi control sequences like "[1;2;3m"
