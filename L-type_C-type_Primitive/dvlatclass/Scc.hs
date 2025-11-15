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
Module      : Scc
Description : Modul zum Rechnen mit Sekundärkegeln
Copyright   : Clara Waldmann, 2014
License     : GPL-2

Modul zum Rechnen mit Sekundärkegeln

-}


module Scc where

import GetVR
import Children
import System.Process
import System.Directory
import System.Exit
import Data.List
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator

{- | Funktion zur Bestimmung der V-Repräsentationen der Sekundärkegel
aus den *.coop-Dateien 


Wird eine Dimension übergeben, werden die *.coop-Dateien mit scc erzeugt.
Ohne Dimension wird scc nicht aufgerufen. Es müssen schon *.coop-Dateien 
im aktuellen Verzeichnis vorhanden sein.
-}
get_sccs :: Maybe Int -> IO [VR]
get_sccs md = do
  case md of
       -- "../SCC-1.0/scc" durch Pfad zum externen Programm scc ersetzen
	Just d -> rawSystem "../SCC-1.0/scc" ["-d", show d, "-w"]
	Nothing -> return ExitSuccess
  files <- getDirectoryContents "."
  mapM  handle $ filter ( isSuffixOf ".coop" ) files
  
  

{- | aus einer *.coop-Datei wird die V-Repräsentation des Sekundärkegels
bestimmt
-}
-- 1. liest Sekundärkegel aus Datei aus
-- 2. Wandelt Darstellung von scc in H-Repräsentation für lrs um
-- 3. Berechnet mit lrs aus H-Rep. die V-Repräsentation
-- 4. liest aus Ausgabe von lrs die V-Repräsentation aus
handle :: FilePath -> IO VR
handle fp = do
  s <- readFile fp
  case runParser cone () "input" s of
        Left err -> error $ show err
        Right c -> do
             vrs <- lrs $ Right $ sc2hr c
             getvr vrs
             
{- | Datenstruktur für Sekundärkegel.
Der Sekundärkegel besteht aus allen Vektoren q, die die Ungleichungen Aq >= 0
erfüllen.
-}
data SC = SC { scdim :: Int,	-- ^ Dimension des Kegels
	       scung :: Int, 	-- ^ Anzahl der Ungleichungen, die den Sekundärkegel beschreiben
	       scmat :: [[Int]] -- ^ Koeffizienten der Ungleichungen A zeilenweise    
	     }
   deriving Show



-- Umwandlung der Kegel-Darstellung von scc in H-Rep. für lrs
-- (H-rep. erwartet Ungleichungen der Form Ax >= b als (-b|A)
--  hier also (0|Kegel-Ungleichungen))
{- | Funktion zur Umwandlung der Kegel-Darstellung von scc in 
H-Repräsentation für lrs

scc liefert Ungleichungen der Form Aq >= 0.
lrs erwartet Ungleichungen der Form Aq >= b als (-b|A).

>sc2hr SC { scdim = 3, scung = 3
>	 , scmat = [[0,-2,0]
>	 	   ,[0,2,2]
>		   ,[2,2,0]
>		   ] 
>	 }
>=     HR { unb = 4, ung = 3
>      	 , hrmat = [[0,0,-2,0]
>		   ,[0,0,2,2]
>		   ,[0,2,2,0]
>		   ]
>         }

-}
sc2hr :: SC -> HR
sc2hr c = HR 
    { unb = scdim c + 1, ung = scung c
    , hrmat = map (\ z -> 0 : z) $ scmat c
    }

{- | liest aus *.coop-Datei die Dimension und die Ungleichungen
des Sekundärkegels aus

> cone  "2
>
>	1
>	0 1 
>	1 1 
>
>	3
>	1 
>	0 0 
>	0 
>	1 0 
>	0 
>	0 1 
>
>	3
>	0 -2 0 
>	0 2 2 
>	2 2 0 
>
>	500
>
>	1e-10"
> = SC { scdim = 3, scung = 3, scmat = [[0, -2, 0], [0, 2, 2], [2, 2, 0]] }

-}
cone :: Parser SC
cone = do
  
    dim <- int
    nsimp <- int

    ms <- count nsimp $ matrix dim dim

    nbas <- int
    bs <- count nbas $ base dim

    c <- sc nbas

    return c
    
{- | eigentlicher Sekundärkegel-Parser
-}
sc :: Int -> Parser SC
sc d = do 
    dim <- int ; m <- matrix dim d
    return $ SC { scdim = d , scung = dim , scmat = m }   

{- | Hilfsparser für quadratische untere Dreiecksmatrizen einer gegebenen Dimension 
-}
base :: Int -> Parser [[Int]]
base dim = sequence $ do
    i <- [ 1 .. dim ]
    return $ count i int
