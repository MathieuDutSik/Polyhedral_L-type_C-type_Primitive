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
Module      : VR2TM
Description : Modul zum Rechnen mit Dreiecksmatrizen
Copyright   : Clara Waldmann, 2014
License     : GPL-2

Modul zum Rechnen mit Dreiecksmatrizen

-}


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}



module VR2TM where

import GetVR
import qualified Numeric.Matrix as M 

-- explizite Determinantenberechnung ist bis zu 5x5-Matrizen 
-- schneller als Determinantenberechnung aus Numeric.Matrix-Paket
import qualified Numeric.Determinant.Explicit as E

{- | Datenstruktur zum speichern symmetrischer Matrizen
als linke untere Dreiecksmatrizen.

> tm_ex = TM { tm_dim = 2, tm_inh = [[2], [-1,2]], tm_det = 3, tm_posdef = True }

>show tm_ex = 
>	2x0
>	2
>	-1 2
-}
data TM = TM { tm_dim :: ! Int		-- ^ Größe der Matrix
     	     , tm_inh :: ! [[Int]]	-- ^ Einträge der Matrix
	     , tm_det :: ! Int		-- ^ Determinante der Matrix
	     , tm_posdef :: ! Bool	-- ^ Aussage über positive Definitheit 
	     }
  deriving Eq

-- | zur Ausgabe von Dreiecksmatrizen als Input für ISOM
instance Show TM where
  show t = unlines 
      [ show ( tm_dim t) ++ "x0"
      , showmat $ tm_inh t
      ]
         
{- | berechnet zu einer V-Repräsentation den Schwerpunkt (Summe der Ecken)
 als Repräsentanten des Kegels.
 Wegen homogener Koordinaten muss die erste Koordinate ignoriert werden.
 
 
 >vr2tm  VR { vr_dim = 3, vr_nvert = 4, dim = 4
 >	  , vr_vert = [[1, 0, 0, 0]
 >		      ,[0, 1, 0, 0]
 >		      ,[0, 0, 0, 1]
 >		      ,[0, 1, -1, 1]
 >		      ]
 >	  }
 >= TM { tm_dim = 2, tm_inh = [[2],[-1,2]], tm_det = 3, tm_posdef = True }
-}
vr2tm :: VR -> TM
vr2tm vr = do 
  let schwpkt = sumarr $ vr_vert vr
  let mv = tail schwpkt
  
  let diml = (takeWhile (\l -> (sum [1..l] <= (dim vr -1))) [1..])     
  let dim = last diml
  
  let m = map (\i -> take i (drop (sum [1..i-1]) mv)) diml
 
-- falls mit Numeric.Matrix-Paket gerechnet wird: 
-- Umwandlung der unteren Dreiecksmatrix in symmetrische Matrix  
--  let symm = M.matrix (dim, dim)
--        $ \(i,j) -> if j <= i then m !! (i-1) !! (j-1) else m !! (j-1) !! (i-1)

-- zur Berechnung der expliziten Determinante muss untere Dreiecksmatrix in
-- symmetrische Matrix umgewandelt werden (in Form von Listen)
  let symm' = case mv of
	-- 5x5-Matrix
        [ a00, a10, a11, a20, a21, a22, a30, a31, a32, a33, a40, a41, a42, a43, a44 ] ->
	    [[a00,a10,a20,a30,a40]
	    ,[a10,a11,a21,a31,a41]
	    ,[a20,a21,a22,a32,a42]
	    ,[a30,a31,a32,a33,a43]
	    ,[a40,a41,a42,a43,a44]
	    ]
	-- 4x4-Matrizen
	[a00, a10, a11, a20, a21, a22, a30, a31, a32, a33] ->
	    [[a00,a10,a20,a30]
	    ,[a10,a11,a21,a31]
	    ,[a20,a21,a22,a32]
	    ,[a30,a31,a32,a33]
	    ]
	-- 3x3-Matrizen
	[a00, a10, a11, a20, a21, a22] ->
	    [[a00,a10,a20]
	    ,[a10,a11,a21]
	    ,[a20,a21,a22]
	    ]
	-- 2x2-Matrizen
	[a00, a10, a11] ->
	    [[a00, a10]
	    ,[a10,a11]
	    ]

-- falls Determinante mit Numeric.Matrix-Paket berechnet wird
--  let d = M.det symm
  let d' = E.det dim symm'
-- falls mit Numeric.Matrix-Paket gerechnet wird
--  let pd = posdef' symm && (d > 0)
  let pd' = and (map (\ i -> E.det i symm' > 0 ) [1..dim-1] ) && d' > 0 
  TM { tm_dim = dim, tm_inh = m,  
-- wenn mit Numeric.Matrix-Paket gerechnet wird
--	tm_det = d, tm_posdef = pd }
      tm_det = d', tm_posdef = pd'}
  
{- | Hilfsfunktion zur Vektoraddition  

> sumarr [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 1, -1, 1]] = [1, 2, -1, 2]
-}
sumarr :: [[Int]] -> [Int]
sumarr = foldl1 $ zipWith (+)	

-- | Testet eine Matrix auf positive Definitheit
-- durch berechnen aller Hauptunterdeterminanten.
posdef' :: M.Matrix Int -> Bool
posdef' m = all (\k -> 0 < M.det ( M.matrix (k,k) (\(i,j) -> M.at m (i,j) ) ) ) [1..M.numCols m -1]
