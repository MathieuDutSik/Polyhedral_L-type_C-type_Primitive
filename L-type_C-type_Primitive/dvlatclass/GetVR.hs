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
Module      : GetVR
Description : Modul zu Berechnungen mit V- und H-Repräsentationen für lrs
Copyright   : Clara Waldmann, 2014
License     : GPL-2

Modul zu Berechnungen mit V- und H-Repräsentationen für lrs

-}


module GetVR where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.List
import Data.Functor.Identity 

{- | Datenstruktur für V-Repräsentationen von Polytopen.
 Das beschriebene Polytop ist konvexe Hülle der gespeicherten Ecken

>vr_ex = VR { vr_dim = 2, vr_nvert = 3, dim = 4
>	   , vr_vert = [[1,0,0,0]
>		       ,[0,1,0,0]
>		       ,[0,1,-1,1]
>		       ]
>	   }

>show vr_ex_dim4 = 
>	V-representation
>	begin
>	3 4 rational
>	1 0 0 0
>	0 1 0 0
>	0 1 -1 1  
>
>	end
>	incidence
	
-}
data VR = VR { vr_dim :: ! Int -- ^ Dimension des Polytops
	     , vr_nvert :: ! Int -- ^ Anzahl der Ecken
	     , dim :: ! Int -- ^ Dimension des Raumes, in dem das Polytop liegt + 1 (wegen homogener Koordinaten)
	     , vr_vert :: [[Int]] -- ^ Liste der Ecken
	     }
  deriving (Eq, Ord)

-- | zur Ausgabe von V-Repräsentationen als Input für lrs
instance Show VR where
    show v = unlines
	[ "V-representation"
	, "begin"
	, unwords [show $ vr_nvert v, show $ dim v, "rational" ]
	, showmat $ vr_vert v
	, "end"
	, "incidence"
	]
	

{- | zur Ausgabe von Matrizen 

>showmat [[1,0,0,0]
>	,[0,1,0,0]
>	,[0,1,-1,1]
>	]
>= 1 0 0 0
>  0 1 0 0
>  0 1 -1 1

-}
showmat :: [[Int]] -> String
showmat = unlines . map ( unwords . map show ) 


{- | liest aus der Ausgabe von lrs angewendet auf eine H-Repräsentation die V-Repräsentation aus

>getvr "*lrs:lrslib v.4.2c, 2010.7.7(32bit,lrsmp.h)
>	*Copyright (C) 1995,2010, David Avis   avis@cs.mcgill.ca 
>	*Input taken from file sc2_0.lrsh
>	V-representation
>	begin
>	***** 4 rational
>	 1  0  0  0 
>	 0  1  0  0 
>	 0  0  0  1 
>	 0  1 -1  1 
>	end
>	*Totals: vertices=1 rays=3 bases=1 integer_vertices=1  vertices+rays=4
>	*lrs:lrslib v.4.2c, 2010.7.7(32bit,lrsmp.h) max digits=8/100
>	*0.000u 0.001s 2068Kb 1 flts 0 swaps 192 blks-in 8 blks-out" 
>= VR { vr_dim = 3, vr_nvert = 4, dim = 4
>     , vr_vert = [[1, 0, 0, 0]
>		 ,[0, 1, 0, 0]
>		 ,[0, 0, 0, 1]
>		 ,[0, 1, -1, 1]
>		 ]
>     }

-}
getvr :: String -> IO VR
getvr s = do
    case runParser verti () "input" s of
        Left err -> error $ show err
        Right vr ->  return vr 


-- | V-Repräsentation-Parser
-- wird aufgerufen in  'getvr'
verti :: Parser VR
verti = do
      -- Dimension einlesen
      d <- anfang 
      -- Ecken-Matrix einlesen
      hm <- manyTill (count d int) (string "end")
      let m = nub hm
      -- Anzahl der Ecken bestimmen
      nv <- anzecken
      return $ VR { vr_dim = d-1, vr_nvert = nv , dim = d , vr_vert = m }

-- | Hilfsparser für 'verti' zur Bestimmung der Anzahl der Ecken
anzecken :: Parser Int      
anzecken = do
      manyTill anyChar $ string "*Totals: "
      string "vertices="; int
      string "rays="; int
      string "bases="; int
      string "integer_vertices="; int
      string "vertices+rays="
      anz <- int;
      return anz

-- | Hilfsparser für 'verti' zur Bestimmung der Dimension
anfang :: Parser Int  
anfang = do  -- Kommentarzeilen, V-representation, begin lesen bis Zahlen los gehen
      manyTill anyChar $ try $ string "*****"
      -- Dimension einlesen
      d <- int 
      string "rational"
      newline
      return d


-- | Int-Parser
int :: Parser Int
int = do i <- integer ; return $ fromInteger i

-- | Hilfsfunktion für 'int'
lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser emptyDef     

-- | Hilfsfunktion für 'int'
integer :: Parser Integer
integer = P.integer lexer

-- | mxn-Matrix-Parser	 
matrix :: Int -> Int -> Parser [[Int]]
matrix dimr dimc = count dimr $ count dimc $ int 
    
    
    
    
{- | Datenstruktur für H-Repräsentationen von Polyedern. 
   Der Polyeder besteht aus allen Punkten x, die die Ungleichungen Ax >= b erfüllen.
   Die Ungleichungen werden dabei in der Form (-b|A) gespeichert.

>hr_ex_dim2 = HR { unb = 4, ung = 3
>		, hrmat = [[0, 0, -2, 0]
>			  ,[0, 0, 2, 2]
>			  ,[0, 2, 2, 0]
>			  ]
>		}

>show hr_ex_dim2 = 
>	H-representation
>	begin
>	3 4 rational
>	0 0 -2 0
>	0 0 2 2
>	0 2 2 0
>
>	end   
   
-}
data HR = HR { unb :: Int		-- ^ Dimension des Raumes, in dem der Polyeder liegt
	     , ung :: Int		-- ^ Anzahl der Ungleichungen, die den Polyeder beschreiben
	     , hrmat :: [[Int]] 	-- ^ Koeffizienten der Ungleichungen (-b|A) zeilenweise
	     }
   -- deriving Show

-- | zur Ausgabe von H-Repräsentationen als Input für lrs
instance Show HR where
    show r = unlines 
        [ "H-representation"
        , "begin"
        , unwords [ show $ ung r, show $ unb r, "rational"]
        , showmat $ hrmat r
        , "end"
        ] 
