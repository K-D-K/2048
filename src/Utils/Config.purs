module Utils.Config where

import Prelude

import Color (Color)
import Data.StrMap (StrMap,insert,empty)

import Utils.ColorSchema (color0, color1024, color128, color16, color2, color2048, color256, color32, color4, color512, color64, color8)
import Utils.Common (Index, Directions(UP,DOWN,LEFT,RIGHT) , FinalCellData , matSize , makeIndex , mkFinalCellData)

colorsPair :: StrMap Color
colorsPair = insert "0" color0 <<< insert "2" color2 <<< insert "4" color4 <<<
			 insert "8" color8 <<< insert "16" color16 <<< insert "32" color32 <<<
			 insert "64" color64 <<< insert "128" color128 <<< insert "256" color256 <<< 
			 insert "512" color512 <<< insert "1024" color1024 <<< insert "2048" color2048 $ empty

nextMove :: Directions -> Index -> Index
nextMove UP 	point = point { x = point.x + 1 }
nextMove DOWN 	point = point { x = point.x - 1 }
nextMove LEFT 	point = point { y = point.y + 1 }
nextMove RIGHT 	point = point { y = point.y - 1 }

isOutOfBounds :: Directions -> Index -> Boolean
isOutOfBounds UP    point 	= eq point.x matSize
isOutOfBounds DOWN  point 	= eq point.x (-1)
isOutOfBounds LEFT  point 	= eq point.y matSize
isOutOfBounds RIGHT point 	= eq point.y (-1)

mkDrawbackFinalCellData :: Directions -> Index -> FinalCellData
mkDrawbackFinalCellData UP 	  point = mkFinalCellData 0 (makeIndex (matSize-1) point.y)
mkDrawbackFinalCellData DOWN  point = mkFinalCellData 0 (makeIndex 0 point.y)
mkDrawbackFinalCellData LEFT  point = mkFinalCellData 0 (makeIndex point.x (matSize-1))
mkDrawbackFinalCellData RIGHT point = mkFinalCellData 0 (makeIndex point.x 0)
