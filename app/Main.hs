module Main where

import Adventurers 
import AdventurersUI

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.ViewPort
import System.Environment

main :: IO ()
main = do
        Just bridge <- loadJuicyPNG "assets/Bridge.png"
        Just lantern <- loadJuicyPNG "assets/Lantern.png"
        [steps, maxTime] <- map (read :: String -> Int) <$> getArgs
        if steps > 8 || steps < 0 || maxTime < 0 then
            putStrLn "The number of steps should be between 1 and 7 and the maximum time should be a positive number" 
        else
            play window background fr (initialState (steps, maxTime) bridge lantern) render keyHandler moveAdvs
