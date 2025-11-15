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


-- Hauptprogramm
-- Tiefensuche im Baum aller Seiten der sekundären Kegel
-- 	Kind von = Facette von
--	Wurzel: scc (1.Generation: sekundäre Kegel)
-- neu (Suche) = arithm. äquivalent, zu schon gefundenen ?

{-|
Module      : Main
Description : Hauptprogramm
Copyright   : Clara Waldmann, 2014
License     : GPL-2

Hauptprogramm
-}

module Main where

import GetVR
import Children
import Isom
import Scc
import VR2TM

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sort)
import Control.Concurrent.Async

{- | Mit Argument (Dimension) werden die Sekundärkegel mit scc berechnet.


   Ohne Argument muss der Aufruf in einem Ordner stattfinden, wo schon *.coop-Dateien liegen 
 			   (scc wird nicht nochmals aufgerufen) 
 			   
 
-}
main :: IO ()
main = do 
  args <- getArgs
  case args of
       [a1] -> main_for_dim $ Just $ read a1
       [] -> main_for_dim Nothing




{- | eigentliche Hauptfunktion

Mit Argument (Dimension) werden die Sekundärkegel mit scc berechnet.


Ohne Argument muss der Aufruf in einem Ordner stattfinden, wo schon *.coop-Dateien liegen 
 			   (scc wird nicht nochmals aufgerufen) 

 			   
Möglichkeiten zur Ausgabe:

* pro Dimension Anzahl der Typen im Format: (Dimension, Anzahl der Typen) 

    Auszug aus der Ausgabe: 
    
    > (5,1681),(6,4366),(7,9255)

* Ausgabe einer Gram-Matrix pro gefundenem Typ 
  (sortiert nach Dimension des Sekundärkegels) mit
  Format: 

    1. Dimension des Sekundärkegels
    2. Format für ISOM
    
    Auszug aus der Ausgabe:
    
    >1 5x0
    >6
    >2 6
    >-2 -2 5
    >-2 -2 1 5
    >-2 -2 -1 -1 5
    >
    >
    >2 5x0
    >2
    >1 2
    >0 0 2
    >-1 -1 -1 2
    >-1 -1 0 0 3
 -}

-- 1. Berechne die sekundären Kegel als 1. Generation
-- 2. Tiefensuche mit Start: todo = sekundäre Kegel, done = []
main_for_dim :: Maybe Int -> IO ()
main_for_dim md = do
  scs <- get_sccs md
  fcs <- enum_faces (S.fromList scs) []
  putStrLn $ unwords [ "Number of Types:", show $ length fcs]
-- zur Ausgabe der Typen pro Dimension (dim, anztypen)
  putStrLn $ show $ dinfo fcs
-- zur Ausgabe der Matrizen (sortiert nach Dimension)
--  forM_ (sort fcs) $ \fc -> putStrLn $ unwords [show ( dimension fc) , show $ vr2tm fc]
  
  
{- | Hilfsfunktion zur Ausgabe der Anzahl der Typen pro Dimension
-}
dinfo :: [VR] -> M.Map Int Int
dinfo vrs = M.fromListWith(+) $ do 
  vr <- vrs
  return (vr_dim vr, 1)





{- | Suche nach erreichbaren Knoten


Möglichkeiten zur Ausgabe:

* in jedem Schritt: Dimensionsliste der bisher gefundenen Typen
  und Dimensionsliste der noch zu prüfenden Typen
* falls neuer Typ gefunden wurde: Ausgabe der Dimension des Sekundärkegels, 
der Gram-Matrix als untere Dreiecksmatrix, Anzahl der Facetten des Sekundärkegels 
-}
-- aus Menge der bisher gesehenen, aber nicht überprüften (todo)
-- und Liste der bisher gefundenen Typen (done)
-- wird Liste von Typen (evtl. mit neuen Typen, evtl. Typen wie bisher) 
-- 1. prüfe ob Element aus todo unbekannt ist (nicht arithmetisch äquiv. zu allen aus done)
-- 2.a ist das Element schon bekannt -> fahre mit restlichen ELementen aus todo fort
-- 2.b ist das Element unbekannt 
--     -> Kinder berechnen und zu todo hinzufügen
--     -> als neuen Typ zu done hinzufügen
--     -> mit neuem todo und neuem done fortfahren
enum_faces :: S.Set VR -> [VR] -> IO [VR]
enum_faces todo done = do
-- Ausgabe in jedem Schritt: 
-- Anzahl der bisher gefundenen Typen gesamt, wie viele pro Dimension
-- Anzahl der noch zu prüfenden Typen gesamt, wie viele pro Dimension
--  putStrLn $ unwords 
--	   [ "enum_faces"
--	   , show $ length done, show $ dinfo done
--	   , show $ S.size todo, show $ dinfo $ S.toList todo
--	   ] 
--  when (length done > 1000) $ error "das reicht"  
  case S.minView todo of
       Nothing    -> return done
       Just(t, odo) -> do 
	 case tm_posdef $ vr2tm t of
	    True -> do
	      u <- unknown t done
	      case u of
		  False -> enum_faces odo done
		  True -> do 
		    cs <- children t
-- Ausgabe falls ein neuer Typ gefunden wurde:
--		    putStrLn 
--		      $ unwords [ "neuer Typ in Dimension:"
--				, show $ dimension t
--				, show $ vr2tm t
--				, "Kinder:"
--				, show $ length cs
--				]
		    enum_faces (S.union (S.fromList cs)  odo) (t:done) 
	    False -> enum_faces odo done
	 
	 
{- | Test ob eine V-Repräsentation unbekannt ist in den bisher bekannten V-Repräsentationen
-}
-- (bekannt = arithmetisch äquivalent)
-- True: wenn v im vgl mit allen aus known unbekannt ist
-- False: wenn v zu mind. einem aus known bekannt ist
-- vgl v mit jedem k aus known und ermittle aus Einzelvergleichen gesamtes Ergebnis
-- ( logisches und der Ergebnisse der Einzelvergleiche (mit disom)  )
unknown:: VR -> [VR] -> IO Bool
unknown v known = parallel_and ( \ k -> not <$> disom v k ) known

{- | Parallele Variante von logischem und auf einer Liste

Prüft, ob alle Elemente in einer Liste eine Eigenschaft erfüllen.
-}
-- bearbeitet Liste xs nicht elementweise, sondern in Blöcken der Größe cores
-- innerhalb der Blöcke findet die Berechnung von f(k) parallel statt
parallel_and :: (a -> IO Bool) -> [a] -> IO Bool  
parallel_and f [] = return True  
parallel_and f xs = do
     let blocksize = 100
         (pre,post) = splitAt blocksize xs
     ys <- mapConcurrently f pre
     case and ys of
	  False -> return False
	  True  -> parallel_and f post
  
  


{- | Prüft, ob alle Elemente in einer Liste eine Eigenschaft erfüllen.
-}
-- liefert ein Element False -> False
-- liefern alle Elemente True -> True
-- ( logisches und auf Liste f(known) )
list_and :: ( a -> IO Bool ) -> [a] -> IO Bool  
list_and f known =  do
  --putStrLn $ unwords [ "unknown", show $ length known]
  case known of
       [] -> return True
       k:nown -> do 
	 d <- f k
	 case d of
	    False -> return False
	    True -> list_and f nown
	    
