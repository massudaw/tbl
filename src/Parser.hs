{-# LANGUAGE OverloadedStrings #-}
module Parser (loadTable) where

import Prelude hiding(takeWhile)
import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)

line v i = manyTill  v i

spacer  = (string " "<|>string "\t")
parseTbl  headerLine = do
  s <- many spacer
  pos <- sepBy  (string "#") (many spacer)
  endOfLine
  header <- manyTill (notChar '\r') endOfLine
  h <- headerLine
  --return  (s,pos,header,h)
  return  (header,h)

expression = do
  ex <- many spacer >> manyTill anyChar (string "->")
  line <- many spacer >> sepBy (realToFrac <$> number) (many spacer)
  endOfLine
  return (BSC.pack ex,line)

expressionTable = do
  sepBy (do
    many spacer
    e <- (realToFrac <$> number)
    many spacer
    line <- many spacer >> sepBy (realToFrac <$> number) (many spacer)
    return (e,line) ) endOfLine


headerLine  = do
  many spacer
  v1 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
  endOfLine
  ex1 <- expression
  many spacer
  v2 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
  endOfLine
  ex2 <- expressionTable
  return $ Table2D v1 ex1 (v2,ex2)

arrayLine = do
  endOfLine
  many spacer
  v1 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
  e1 <- many spacer >> sepBy (realToFrac <$> number) (many spacer)
  endOfLine
  -- empty line
  many spacer
  endOfLine
  -- empty line
  many spacer
  v2 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
  e2 <- many spacer >> sepBy (realToFrac <$> number) (many spacer)
  return (TableArray (v1 ,e1) (v2,e2))


meta = do
  TableMeta <$>  sepBy (many (notChar '\r')) endOfLine

data TableType
  = Table2D ByteString (ByteString,[Double]) (ByteString,[(Double,[Double])])
  | TableArray (ByteString,[Double]) (ByteString,[Double])
  | TableMeta [String]
  deriving(Eq,Ord,Show)

tables = do
  tb <- sepBy (parseTbl (headerLine <|> arrayLine )) (endOfLine >> many (string "=")>> endOfLine)
  (endOfLine >> many (string "=")>> endOfLine)
  m <- meta
  return (tb,m)

main = loadTable  "Sr5-5.tbl"

loadTable i = do
  f <- BS.readFile i
  return $ case (parseOnly tables f ) of
            Right i -> i
            Left j -> error j





