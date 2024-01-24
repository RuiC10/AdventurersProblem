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
        play window background fr (initialState (steps, maxTime) bridge lantern) render keyHandler moveAdvs
