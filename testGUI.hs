import System.IO
import System.Random
import Control.Monad
import Graphics.Gloss

import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game 

import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Rendering

import Graphics.Gloss.Data.Color

window = InWindow "Hangman" (640 , 480) (10,10)

{-

pic = Color myColor $ rectangleSolid 50 50

cirkel = Color myColor $ circleSolid  50 

myColor = makeColor 0.1 0.6 0.4 1
--}


type World = (Float, Float)

type Model = (Float, Float)




main = simulate
    window
    white
    simulationRate
    initialModel
    drawingFunc
    updateFunc
    where
        simulationRate :: Int
        simulationRate = 20

        initialModel :: Model
        initialModel = (0,0)

        drawingFunc :: Model -> Picture 
        drawingFunc (theta, dtheta) = line [(30, -30), (60, 0)]

        updateFunc :: ViewPort -> Float -> Model -> Model
        updateFunc _ dt (theta, dtheta) = (theta + dt * (20 * dtheta), dtheta - dt * (cos theta))

drawing1 = [translate (-20) (-100) $ color green $ circleSolid 100,
            translate (-20) (-150) $ color white $ rectangleSolid 200 100]

drawing2 = drawing1 ++ [translate (-20) (100) $ color black $ rectangleSolid 10 200 ]

drawing3 = drawing2 ++ [translate (15) (200) $ color black $ rectangleSolid 75 10 ]

drawing4 = drawing3 ++ [translate (50) (180) $ color black $ rectangleSolid 2 30,
                        translate (50) (140) $ color black $ circle 20]

drawing5 = drawing4 ++ [translate (50) (97) $ color black $ rectangleSolid 1 50,
                        translate (110) (46) $ color black $ line [(-30, -30), (-60, 30)],
                        translate (-10) (46) $ color black $ line [(30, -30), (60, 30)]]

drawing6 = drawing5 ++ [translate (110) (110) $ color black $ line [(-30, -30), (-60, 0)], 
                        translate (-10) (110) $ color black $ line [(30, -30), (60, 0)]]
{-


animationFunc :: Float -> Picture
animationFunc time = Circle (2 * time)

drawingFunc :: World -> Picture
drawingFunc (x, y) = translate x y (Circle 20)

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 10)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 10)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 10, y)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 10, y)
inputHandler _ w = w


updateFunc :: Float -> World -> World
updateFunc _ (x, y) = (towardCenter x, towardCenter y)
  where
    towardCenter :: Float -> Float
    towardCenter c = if abs c < 0.25
      then 0
      else if c > 0
        then c - 1.25
        else c + 1.25

main3 :: IO ()
main3 = play
  window
  white
  20
  (0, 0)
  drawingFunc
  inputHandler
  updateFunc

poly = color black $ polygon [((-10), 10), ((-10), 70), (20, 20), (20, 80)]

line1 = color green $ translate (-200) 100 $ line  [(-30, -30), (-40, 30), (30, 40), (50, -20)]

textPic = center $ reScale $ color green $ Text string1

pics = [textPic,poly]

width = 640
height = 480

renderFrame window glossState = do
    displayPicture (width, height) white glossState 1.0 $
        Pictures
            [ Color violet $ translate (-300) 100 $
            polygon [((-10), 10), ((-10), 70), (20, 20), (20, 30)]
            , Color red $ translate (-200) 100 $
            line [(-30, -30), (-40, 30), (30, 40), (50, -20)]
            , Color (makeColor 0 128 255 1) $ translate (-100) 100 $
            lineLoop [(-30, -30), (-40, 30), (30, 40), (50, -20)]
            , Color red $ translate 0 100 $
            circle 30
            , Color green $ translate 100 100 $
            thickCircle 30 10
            , Color yellow $ translate 200 100 $
            circleSolid 30
            , Color chartreuse $ translate (-200) (-100) $
            thickArc 0 180 30 30
            , Color (dark magenta) $ translate (-100) (-100) $
            arcSolid 0 90 30
            , Color (bright magenta) $ translate 0 (-100) $ scale 0.2 0.2 $
            text "Boo!"
            , Color (dim cyan) $ translate 100 (-100) $ rotate 30 $
            rectangleWire 20 50
            , Color (light cyan) $ translate 200 (-100) $ rotate 60 $
            rectangleSolid 20 50 ]

main2 :: IO ()
main2 = display window background drawing
    where
        background = white 
        drawing = line1



center = translate (-(halfSize)) (0)

reScale = scale 0.2 0.2

halfSize :: Float
halfSize = realToFrac $ (length string1) `div` 2 * 15

string1 :: String
string1 = "EDVARD is sketch"

--Color (bright magenta) $ translate 0 (-100) $ scale 0.2 0.2 $
--text "Boo!"
-}