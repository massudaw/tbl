{-# LANGUAGE DeriveFunctor,OverloadedStrings #-}
module TBL.Parser (TableType(..),loadTable,constantInterp,linearInterp ,bilinearInterp ,trilinearInterp,buildTable2D,buildTable0D,buildTable1D ,buildTable3D,tbl) where

import GHC.Stack
import Prelude hiding(takeWhile)
import Control.Exception
import Debug.Trace
import System.Directory
import Data.Monoid
import qualified Data.List as L
import Linear.V2
import Linear.Metric
import Linear.Matrix
import qualified Data.Map as M
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding(number)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)

line v i = manyTill  v i
number = (do
         scientific
         char '('
         mm <- scientific
         char ')'
         return mm ) <|> scientific

spacer  = (string " "<|> string "\t")

parseTbl  headerLine = do
  s <- many spacer
  pos <- sepBy  (string "#") (many spacer)
  many spacer >> endOfLine
  many spacer
  header <- manyTill anyChar endOfLine
  c <- headerLine
  return  (header,c)

expression = do
  ex <- many spacer >> manyTill anyChar ("->")
  line <- many spacer >> sepBy (realToFrac <$> number) (many spacer)
  endOfLine
  return (BSC.pack ex,line)

expressionTable = do
  sepBy (do
    many spacer
    e <- (realToFrac <$> number)
    line <- many spacer >> sepBy (realToFrac <$> number) (many spacer)
    return (e,line) ) (many spacer >> endOfLine)

expressionTable3d = do
  sepBy (do
    many spacer
    a <- realToFrac <$> number
    l <- expressionTable
    return (a,l)) ( many spacer >> endOfLine >> many (many spacer >> endOfLine))


table3d = do
  many spacer
  v1 <- takeTill  (\i -> i `elem` ("\r\n" :: String))
  endOfLine
  ex1 <- expression
  many spacer
  v2 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
  many spacer
  v3 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
  endOfLine
  ex2 <- expressionTable3d
  return $ Table3D v1 ex1 (v2,v3,ex2)


headerLine  = do
  v1 <- (do
    many spacer
    v1 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
    endOfLine
    return v1 ) <|> (return "cix")
  ex1 <- expression
  many spacer
  v2 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
  endOfLine
  ex2 <- expressionTable
  return $ Table2D v1 ex1 (v2,ex2)

constant = do
  -- many spacer >> endOfLine
  _ <- manyTill anyChar  endOfLine
  many spacer
  v1 <- takeTill  (\i -> i `elem` ("=" :: String))
  string "="
  many spacer
  n <- number
  return (TableConstant v1 (realToFrac n) )

express = BSC.pack <$> (manyTill anyChar ("in.(mm)" <|> "->" )) <|>takeTill  (\i -> i `elem` ("\r\n " :: String))

arrayLine = do
  many1 endOfLine
  many spacer
  v1 <- express -- takeTill  (\i -> i `elem` ("\r\n " :: String))
  e1 <- many spacer >> sepBy (realToFrac <$> number) (many spacer)
  endOfLine
  -- empty line
  many spacer
  many1 endOfLine
  -- empty line
  many spacer
  v2 <- takeTill  (\i -> i `elem` ("\r\n " :: String))
  e2 <- many spacer >> sepBy (realToFrac <$> number) (many spacer)
  return (TableArray (v1 ,e1) (v2,e2))


meta = do
  TableMeta <$>  sepBy1 (manyTill anyChar endOfLine ) endOfLine

data TableType a
  = Table2D ByteString (ByteString,[a]) (ByteString,[(a,[a])])
  | Table3D ByteString (ByteString,[a]) (ByteString,ByteString,[(a,[(a,[a])])])
  | TableArray (ByteString,[a]) (ByteString,[a])
  | TableConstant ByteString a
  | TableMeta [String]
  deriving(Eq,Ord,Show,Functor)

tables = do
  let sep = (many spacer >> endOfLine >> many (string "=")>> endOfLine)
  tb <- sepBy1 (parseTbl ( headerLine <|> constant <|>arrayLine <|> table3d  ))  sep
  sep
  m <- meta
  return (tb,m)
  -- return (tb,undefined)

main = loadTable  "Er5-6.tbl"

loadTable i = do
  f <- BS.readFile i
  return $ case (parseOnly tables f ) of
            Right i -> Right i
            Left j -> Left $ (i)


tbl directory = do
  files <- getDirectoryContents directory
  i <-mapM (\i -> do
           (fmap (\(a,b)-> (fst $ break (=='.') $ i,a) ) <$> loadTable (directory <>"/" <> i)) ) ({-filter (`elem` ["Sr5-5.tbl","Er1-3.tbl","Er1-2.tbl","Cd9-1.tbl","Ed5-1.tbl"]) $-} filter (\i -> L.isInfixOf  ".tbl" i ) files)
  return $ i

trilinearInterp v@(x,z,k) m
  | otherwise = do
    ((x1,z1,k1), y111) <- M.lookupLT v m
    ((x2,z2,k2), y222) <- M.lookupGT v m
    y112 <- M.lookup (x1,z1,k2) m
    y221 <- M.lookup (x2,z2,k1) m
    y121 <- M.lookup (x1,z2,k1) m
    y122 <- M.lookup (x1,z2,k2) m
    y211 <- M.lookup (x2,z1,k1) m
    y212 <- M.lookup (x2,z1,k2) m
    let b1 = bilinear (x,z) (x1,z1,x2,z2) (y111,y121,y211,y221)
    let b2 = bilinear (x,z) (x1,z1,x2,z2) (y112,y122,y212,y222)
    return $ linear k (k1,k2) (b1,b2)

linear x (x1,x2) (y1,y2) = y1  + (y2-y1)/(x2 - x1)*(x - x1)

bilinear (x,z) (x1,x2,z1,z2) (y11,y12,y21,y22) = (1/(x2-x1)/(z2-z1) ) * ((V2  (x2 - x) (x - x1)) `dot` ((V2 (V2 y11 y12) (V2 y21 y22)) !* V2 (z2 - z) (z-z1)))

bilinearInterp v@(x,z) m
  | otherwise = do
    ((x1,z1), y11) <- M.lookupLT v m
    ((x2,z2), y22) <- M.lookupGT v m
    y12 <- M.lookup (x1,z2) m
    y21 <- M.lookup (x2,z1) m
    return $ bilinear (x,z) (x1,x2,z1,z2) (y11,y12,y21,y22)
  where gt = M.lookupGT v m
        lt = M.lookupLT v m

constantInterp m = m

linearInterp x m = M.lookup x m <|> (do
  (x1,y1) <- M.lookupLT x m
  (x2,y2) <- M.lookupGT x m
  return $ linear x (x1,x2) (y1,y2))


buildTable0D (TableConstant ax ay) = ay
buildTable1D (TableArray (_,ax) (_,ay))
  = M.fromList $  zipWith (\i cb -> (i,cb) ) ax ay
buildTable1D i = errorWithStackTrace (show ("1D",i))
buildTable2D (Table2D n arr tmap )
  = M.fromList $ concat $ fmap (\(ix,l) -> zipWith (\cb i-> ((i,ix),cb) ) l (snd arr)) (snd tmap)
buildTable2D i = errorWithStackTrace (show ("2D",i))
buildTable3D (Table3D n (_,arr) (_,_,tmap) )
  = M.fromList $ concat $ concat $ fmap (\(ix1,l1) -> fmap (\(ix,l) -> zipWith (\cb i-> ((i,ix1,ix),cb) ) l arr) l1) tmap
buildTable3D i = errorWithStackTrace (show ("3D",i))




