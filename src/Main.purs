module Main where

import Prelude

import Color (lighten, fromHexString,rgb)
import Color.Scheme.X11 (antiquewhite, aqua, aliceblue, brown, coral, darkred)
import Color.Scheme.MaterialDesign (lime,red,grey)
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
import Graphics.Drawing (Drawing , Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, render, scale, translate, text, Font,clipped)
import Graphics.Drawing.Font
import Partial.Unsafe (unsafePartial)
foreign import logAny :: forall a.a -> Unit

type Circle = { x :: Number, y :: Number, size :: Number }
type Square = { x :: Number , y :: Number , w :: Number , h :: Number, val :: Int}

makeDimensions :: Int -> Int -> Array Point
makeDimensions x y = let arrRange = (0 .. matSize) in concat $ map (makeRow arrRange) arrRange
  where
    makeRow :: Array Int -> Int -> Array Point
    makeRow arrRange rowIndex = map (makeCol rowIndex) arrRange
    makeCol :: Int -> Int -> Point
    makeCol rowIndex colIndex = { x : (toNumber (x + rowIndex * squareWidth)), y : (toNumber (y + colIndex * squareWidth)) }

matSize :: Int
matSize = 3

squareWidth :: Int
squareWidth = 60

colorR :: Array Int
colorR = [1,2,3,4,5]

colorG :: Array Int
colorG = [1,2,3,4,5]

colorB :: Array Int
colorB = [1,2,3,4,5]

scene :: { w :: Number, h :: Number } -> Array Point -> Behavior Drawing
scene { w, h } dimensions = pure background <> map renderSquares squares where
  background :: Drawing
  background = filled (fillColor antiquewhite) (rectangle 0.0 0.0 w h)

  
  renderSquare :: Square -> Drawing
  renderSquare { x, y, w , h , val } =
      outlined
        (outlineColor rgb colorR[val] colorG[val] colorB[val] <> lineWidth (5.0))
        (rectangle x y w h)  
     

  renderSquares :: Array Square -> Drawing
  renderSquares = foldMap renderSquare

  squares :: Behavior (Array Square)
  squares = toCircles <$> keys
    where
        toCircles m = let abc = logAny keys in map mkSquareCo_ordinates dimensions
        mkSquareCo_ordinates point = { x : point.x , y :  point.y , w : (toNumber squareWidth) , h : (toNumber squareWidth), val: 1}

main :: forall eff. Eff (canvas :: CANVAS, frp :: FRP | eff) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth w canvas
  _ <- setCanvasHeight h canvas
  _ <- animate (scene { w, h } (makeDimensions 55 55)) (render ctx)
  pure unit
