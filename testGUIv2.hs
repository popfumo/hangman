
module Main(main) where
import System.IO
import System.Random
import Control.Monad
import Graphics.Gloss

import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game 

import Graphics.Gloss.Data.Picture


import Graphics.Gloss.Data.Color

window :: Display
window = InWindow "Hangman" (1000 , 700) (10,10)

background :: Color
background = white

main :: IO ()
main = display window background $ printDrawing drawing6

circle1 = color blue $ circleSolid 40


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

printDrawing xs = pictures xs