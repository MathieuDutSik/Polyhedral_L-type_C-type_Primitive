--    Copyright 2014, Clara Waldmann

--    This file is part of dvlatclass.
-- 
--    dvlatclass is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation version 2 of the License.
-- 
--    dvlatclass is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
-- 
--    You should have received a copy of the GNU General Public License
--    along with dvlatclass.  If not, see <http://www.gnu.org/licenses/>.


{-|
Module      : Children
Description : Modul zur Berechnungen der Kinder (Facetten) eines Knotens
Copyright   : Clara Waldmann, 2014
License     : GPL-2

Modul zur Berechnungen der Kinder (Facetten) eines Knotens
-}
 

module Children where

import GetVR
import VR2TM

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import System.Process
import System.IO
import Data.List
import Data.Maybe


{- | berechnet Liste der Facetten einer V-Repräsentation mittles lrs

> children VR { vr_dim = 3, vr_nvert = 4, dim = 4
>             , vr_vert = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 1, -1, 1]]
>             }
> = [VR { vr_dim = 2, vr_nvert = 3, dim = 4, vr_vert = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1]]}
>   ,VR { vr_dim = 2, vr_nvert = 3, dim = 4, vr_vert = [[1, 0, 0, 0], [0, 0, 0, 1], [0, 1, -1, 1]]}
>   ,VR { vr_dim = 2, vr_nvert = 3, dim = 4, vr_vert = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 1, -1, 1]]}
>   ]

-}
-- 1. rufe lrs für v auf
-- 2. lies aus Ausgabe von lrs die Facetten aus
children :: VR -> IO [VR]
children v = do
    fs <- lrs $ Left v
    return $ getfcs fs v
    
    
    
-- | Aufruf von lrs für V-Repräsentation oder H-Repräsentation
lrs :: Either VR HR -> IO String
lrs e = do
  -- "../lrslib-042c/lrs" durch Pfad zum externen Programm lrs ersetzen
  let lrs = "../lrslib-042c/lrs"
  let inp = case e of
		Left vr -> show vr
		Right hr -> show hr
  out <- readProcess lrs [] inp
  return out
  
-- test_getfcs_dim2 = do s <- lrs $ Left vr_ex_dim2; return $ getfcs  s vr_ex_dim2

{- | liest aus Ausgabe von lrs angewendet auf eine V-Repräsentation mit Option @incidence@ 
 die Facetten aus
 
>getfcs "*lrs:lrslib v.4.2c, 2010.7.7(32bit,lrsmp.h)
>	*Copyright (C) 1995,2010, David Avis   avis@cs.mcgill.ca 
>	*incidence
>	H-representation
>	begin
>	***** 4 rational
>	F#1 B#1 h=0 vertices/rays  1* 2 3 4 I#3 det= 1  in_det= 1 
>	 1  0  0  0 
>	F#2 B#1 h=0 vertices/rays  1 2 3 4* I#3 det= 1  in_det= 1 
>	 0  0 -1  0 
>	F#3 B#1 h=0 vertices/rays  1 2* 3 4 I#3 det= 1  in_det= 1 
>	 0  1  1  0 
>	F#4 B#1 h=0 vertices/rays  1 2 3* 4 I#3 det= 1  in_det= 1 
>	 0  0  1  1 
>	end
>	*Totals: facets=4 bases=1
>	*lrs:lrslib v.4.2c, 2010.7.7(32bit,lrsmp.h) max digits=8/100
>	*0.001u 0.001s 2004Kb 0 flts 0 swaps 0 blks-in 0 blks-out" 
>      VR {dim = 3, vr_nvert = 4, vr_dim = 4, vr_vert = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 1, -1, 1]]}
> = [VR { vr_dim = 2, vr_nvert = 3, dim = 4, vr_vert = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1]]}
>   ,VR { vr_dim = 2, vr_nvert = 3, dim = 4, vr_vert = [[1, 0, 0, 0], [0, 0, 0, 1], [0, 1, -1, 1]]}
>   ,VR { vr_dim = 2, vr_nvert = 3, dim = 4, vr_vert = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 1, -1, 1]]}
>   ]

 
-}
-- ( lrs liefert pro Facette eine Liste der Indices der Ecken von vr, 
--   die auf dieser Facette liegen )
-- 1. Liste der Index-Listen lesen ( für jede Facette Liste der Ecken )
-- 2. aus den Index-Listen und vr Facetten berechnen 
getfcs :: String -> VR -> [VR]
getfcs inp vr = do
    case runParser facets () "input" inp of
        Left err -> error $ show err
        Right fcsind -> facetsfromind fcsind vr


-- | Parser für Liste der Indexlisten der Facetten wird aufgerufen in 'getfcs'
facets :: Parser [[Int]]
facets = do
      manyTill anyChar $ try $ string "*****"
      d <- int
      fs <- manyTill facet $ string "end"
      return fs
    
-- | Hilfsparser für 'facets' für eine Facette als Liste von Indices 
facet :: Parser [Int]
facet = do
      manyTill anyChar $ string "F#"
      int
      string "B#"; int
      string "h="; int
      string "vertices/rays  "
      indstr <- manyTill anyChar $ string "I"
      let ind = eckind $ delete 'I' indstr
      count 2 $ manyTill anyChar newline
      return ind
      
{- | Hilfsparser für 'facet' für eine Liste von Indices

>eckind " 1* 2 3 : 4 5 " = [2,3,4,5]
-}
eckind :: String -> [Int]
eckind str = 
      map (\e -> read e :: Int ) ( filter (\i -> (last i) /= '*') $ filter (/= ":") $ words str)


{- | Bestimmt aus der Liste der Indices die Facetten als V-Repräsentationen
-}
-- 1. bestimme "richtige" Facetten ( müssen 0-Punkt enthalten ! ) bzw. deren Indices
-- 2. berechne aus "richtigen" Index-Listen die Facette
facetsfromind :: [[Int]] -> VR -> [VR]
facetsfromind inds vr = 
	  -- Facette muss 0-Punkt enthalten !
	  -- als "Facette" kommt auch gesamte vr raus (ohne 0-Punkt), wegen homogener Koordinaten
	  let rinds = filter (\i -> elem (1:replicate (dim vr -1) 0) (map (\j -> (vr_vert vr) !! (j-1)) i )) inds 
	  in map (\i -> facetfromind i vr) rinds


{- | Hilfsfunktion für 'facetsfromind' zur Bestimmung einer Facette aus der Indexliste
-}
-- als Facette ist die Dimension des Objekts eins kleiner als die Dimension der vr
-- Anzahl der Ecken ist Anzahl der Indices
-- Dimension des Raumes bleibt erhalten
facetfromind :: [Int] -> VR -> VR
facetfromind ind vr = 
  let hvrv = map (\i -> (vr_vert vr) !! (i-1)) ind
  in  VR { vr_dim = vr_dim vr - 1 , vr_nvert = length hvrv, dim = dim vr, vr_vert = hvrv }

