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
Module      : Isom
Description : Modul zum Vergleich der Knoten auf Äquivalenz
Copyright   : Clara Waldmann, 2014
License     : GPL-2

Modul zum Vergleich der Knoten auf Äquivalenz
-}


module Isom where

import GetVR
import VR2TM
import System.Process
import System.IO
import System.Exit
import Data.Char

{- | Test auf Äquivalenz zweier V-Repräsentationen. 
 Liefert 'True' wenn die V-Repräsentationen äquivalent sind, sonst 'False'. 
 Erst werden alle Invarianten (Dimension des Polytops, 
 Determinante des Schwerpunkts, Anzahl der Kanten des Kegels) 
 überprüft, dann wenn notwendig ISOM aufgerufen.
 
 Beipsiel für V-Repräsentationen mit verschiedener Dimension
 (ISOM wird nicht aufgerufen)
 
 >disom VR { vr_dim = 3, vr_nvert = 3, dim = 4
 >	 , vr_vert = [[0,1,0,0], [0,0,0,1], [0,1,-1,1]] 
 >	 } 
 >      VR { vr_dim = 2, vr_nvert = 2, dim = 3
 >	 , vr_vert = [[0,1,0,0], [0,0,0,1]]
 >	 }
 >= False
 
 Beispiel für äquivalente V-Repräsentationen 
 (nach Test der Invarianten wird ISOM aufgerufen)

 >disom VR { vr_dim = 2, vr_nvert = 2, dim = 3
 >	 , vr_vert = [[0,1,0,0], [0,0,0,1]]
 >	 }
 >      VR { vr_dim = 2, vr_nvert = 2, dim = 3
 >	 , vr_vert = [[0,0,0,1], [0,1,-1,1]]
 >	 }
 >= True
 
-}
disom :: VR -> VR -> IO Bool
disom v1 v2 = case vr_dim v1 == vr_dim v2 of
		      False -> return False
		      True -> case (tm_det (vr2tm v1)) == ( tm_det (vr2tm v2) ) of
				False -> return False
				True -> case (vr_nvert v1) == (vr_nvert v2) of
						False -> return False
						True -> isom v1 v2 


{- | Test auf Äquivalenz zweier V-Repräsentationen
durch Test der Grammatrizen, die durch die Schwerpunkte der V-Repräsentationen gegeben sind, 
auf arithmetische Äquivalenz mittels ISOM.
 Liefert 'True', wenn die Grammatrizen arithmetisch äquivalent sind, sonst 'False'.
-}
-- 1. berechne aus V-Rep. die entsprechenden Gram-Matrizen
-- 2. rufe ISOM für die Grammatrizen auf
-- 3.a sind Matrizen äquiv. 
-- 	-> ISOM liefert Transformationsmatrix (also nur Zahlen in 1. Zeile)
-- 	-> True
-- 3.b Matrizen nicht äquiv.
-- 	-> ISOM macht Text-Ausgabe (also nicht nur Zahlen in 1. Zeile)
-- 	-> False
isom :: VR -> VR -> IO Bool
isom v1 v2 = do
  let t1 = vr2tm v1
  let t2 = vr2tm v2
  -- "../ISOM_and_AUTO/ISOM" durch Pfad zum externen Programm ISOM ersetzen
  let isomp = "../ISOM_and_AUTO/ISOM"
  -- ISOM erwartet #Anz -> sonst werden die Files nicht richtig geschlossen (fseek....)
  let inp = "#1\n" ++ show t1 ++ show t2
  (ex, out, err) <- readProcessWithExitCode isomp [] inp
  case ex of
       ExitFailure 3 -> return True -- Error: nicht pos. def.
       _ -> case lines out of
			z : _	| all isDigit z -> return True
			_ -> return False