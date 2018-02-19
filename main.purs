module Main where

import Prelude

import Color (lighten)
import Color.Scheme.MaterialDesign (lime,red)
import Control.Monad.Eff (Eff)
import Data.Array (sortBy, (..),concat)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (fromJust, maybe)
import Data.Set (isEmpty)
import FRP (FRP)
import FRP.Behavior (Behavior, animate, fixB, integral', switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Mouse as Mouse
import FRP.Behavior.Time as Time
import FRP.Event.Class (fold)
import FRP.Behavior.Keyboard (keys)
import Global (infinity)
import Graphics.Canvas (CANVAS, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing , Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, render, scale, translate)
import Partial.Unsafe (unsafePartial)

type Circle = { x :: Number, y :: Number, size :: Number }
type Square = { x :: Number , y :: Number , w :: Number , h :: Number}

makeDimensions :: Int -> Int -> Array Point
makeDimensions x y = let arrRange = (0 .. matSize) in concat $ map (makeRow arrRange) arrRange
  where
    makeRow :: Array Int -> Int -> Array Point
    makeRow arrRange rowIndex = map (makeCol rowIndex) arrRange
    makeCol :: Int -> Int -> Point
    makeCol rowIndex colIndex = { x : (toNumber (x + rowIndex * squareWidth)), y : (toNumber (y + colIndex * squareWidth)) }

matSize :: Int
matSize = 4

squareWidth :: Int
squareWidth = 40

scene :: { w :: Number, h :: Number } -> Array Point -> Behavior Drawing
scene { w, h } dimensions = pure background <> map renderSquares squares where
  background :: Drawing
  background = filled (fillColor lime) (rectangle 0.0 0.0 w h)

  renderSquare :: Square -> Drawing
  renderSquare { x, y, w , h } =
    outlined
        (outlineColor red <> lineWidth (1.0))
        (rectangle x y w h)

  renderSquares :: Array Square -> Drawing
  renderSquares = foldMap renderSquare

  squares :: Behavior (Array Square)
  squares = toCircles <$> keys
  	where
        toCircles m = map mkSquareCo_ordinates dimensions
        mkSquareCo_ordinates point = { x : point.x , y :  point.y , w : (toNumber squareWidth) , h : (toNumber squareWidth)}

main :: forall eff. Eff (canvas :: CANVAS, frp :: FRP | eff) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth w canvas
  _ <- setCanvasHeight h canvas
  _ <- animate (scene { w, h } (makeDimensions 0 0)) (render ctx)
  pure unit
