module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.ViewPort
import Adventurers
import Data.List
import Data.Maybe
import DurationMonad
import Data.Either
import System.Environment

----------------------------------------------------------------------------------------

data AdvSim = Sim 
    { bridgeImage :: Picture
    , lanternImage :: Picture
    , trace :: LogListDur State
    , currentTrace :: Int
    , currentLog :: Int
    , crossingDirection :: Bool
    , advCrossing :: [Int]
    , crossingSpeed :: Float
    , crossingDistance :: Float
    , elapsedTime :: Float
    , args :: (Int, Int)
    , started :: Bool
    } deriving Show

numAdv :: Float 
numAdv = fromIntegral $ length adventurers

windowDim :: (Int, Int)
windowDim = (1280, 720)

bridgeSize :: Float
bridgeSize = 700.0

window :: Display
window = InWindow "Adventurers" windowDim (0, 0)

background :: Color
background = black

fr :: Int
fr = 30

sizeAdv :: Int
sizeAdv = 60

helper :: [String]
helper = ["KEYS", "l -> Next Trace","k -> Increase speed","j -> Decrease speed","r -> Reset Trace", "i -> Start simulation"]

----------------------------------------------------------------------------------------

genCoordinatesLeft :: Int -> [(Float, Float)]
genCoordinatesLeft n = take n [(x1, y1) | x1 <- x, y1 <- y]
    where
        z = sqrt(fromIntegral n)
        x = [-600, -600 + (120 / fromIntegral n) .. (-460)]
        y = [80, 80 - (120 / fromIntegral n) .. (-40)]

genCoordinatesRight :: Int -> [(Float, Float)]
genCoordinatesRight n = take n [(x1, y1) | x1 <- x, y1 <- y]
    where
        z = sqrt(fromIntegral n)
        x = [600, 600 - (120 / fromIntegral n) .. (460)]
        y = [-80, -80 + (120 / fromIntegral n) .. (40)]

genCoordinatesCross :: Int -> Float -> Bool -> [(Float, Float)]
genCoordinatesCross n dist dir = take n [(x1, y1) | x1 <- x, y1 <- y]
    where
        z = sqrt(fromIntegral n)
        x | dir = replicate n (-(bridgeSize/fromIntegral(2)) + dist)
          | otherwise = replicate n (bridgeSize/fromIntegral(2) - dist)
        y = [-60, -60 + (120 / fromIntegral n) .. (100)]

makeAdventurer :: (String, (Float, Float)) -> Picture
makeAdventurer (str,(x,y)) = translate x y $ Pictures [adv, txt]
    where
        adv = Pictures [circleSolid (65 / numAdv), color white $ circleSolid (60/numAdv)]
        txt = translate (-17/numAdv) 0 $ scale (numAdv / 40) (numAdv / 40) $ text str

makeStationaryAdvs :: [(Bool,Adventurer)] -> Picture
makeStationaryAdvs stAdvs = Pictures $ map makeAdventurer allStationaryAdvs  
    where
        (st, advs) = unzip stAdvs
        leftSide = map (show . getTimeAdv . snd) $ filter (\(a,b)-> not a) $ zip st advs
        rightSide = map (show . getTimeAdv . snd) $ filter (\(a,b)-> a) $ zip st advs
        leftCoordinates | length leftSide > 0 =  genCoordinatesLeft (length leftSide)
                        | otherwise = []
        rightCoordinates | length rightSide > 0 =  genCoordinatesRight (length rightSide)
                         | otherwise = []
        allStationaryAdvs = zip leftSide leftCoordinates ++ zip rightSide rightCoordinates

makeCrossingAdvs :: Float -> Bool -> [Adventurer] -> Picture
makeCrossingAdvs dist dir ad = Pictures $ map makeAdventurer allCrossingAdventurers
    where
        coordinates = genCoordinatesCross (length ad) dist dir
        advs = map (\p -> show . getTimeAdv $ p) ad
        allCrossingAdventurers = zip advs coordinates
        
makeCrossingLantern :: Bool -> Float -> Picture -> Picture
makeCrossingLantern dir dist p | dir = translate (-(bridgeSize / fromIntegral(2)) + dist + 40) (-30) $ p
                               | otherwise = translate ((bridgeSize / fromIntegral(2)) - dist - 40) (-30) $ rotate 180 p

makeStationaryLantern :: Bool -> Picture -> Picture
makeStationaryLantern dir p | dir = translate (-500) 0 p
                            | otherwise = rotate 180 $ translate (-500) 0 p
        
makeText :: (Float, Float) -> [(String, (Float, Float))] -> Picture
makeText (s1, s2) = Pictures . map (\(str,(x,y)) -> translate x y $ scale s1 s2 $ text str)

render :: AdvSim -> Picture
render sim | (length . remLog . trace $ sim) == 0 = Pictures $ [bridgeImage sim, helperPic, timePic]
           | otherwise = Pictures $ [bridgeImage sim, stationaryAdvs, movingAdvs,lantern, helperPic, timePic]
    where 
        cLog = (!! (currentLog sim)) . fst . (!! (currentTrace sim)) . remLog . trace $ sim
        sideAdvs = map cLog adventurers
        advs = zip [0..(length adventurers)-1] $ zip sideAdvs (map (\(Left s) -> s) adventurers)
        stationaryAdvs = makeStationaryAdvs $ map snd $ filter (\(a,b) -> not (a `elem` (advCrossing sim))) advs
        movingAdvs = makeCrossingAdvs (crossingDistance sim) (crossingDirection sim) $ map (snd . snd) $ filter (\(a,b) -> a `elem` (advCrossing sim)) advs 
        lantern | (advCrossing sim) == [] = makeStationaryLantern (crossingDirection sim) (lanternImage sim)  
                | otherwise = makeCrossingLantern (crossingDirection sim) (crossingDistance sim) (lanternImage sim)  
        helperPic = makeText (0.2,0.2) $ zip helper (zip (replicate (length  helper) (-150)) [320.0, 280.0 .. (320.0 - 40.0 * (fromIntegral . length $ helper))])
        timePic = makeText (0.5, 0.5) $ zip ["TIME ", show . elapsedTime $ sim] $ zip [-150.0, 30.0] (replicate 2 (-300.0))

----------------------------------------------------------------------------------------

finishCrossing :: AdvSim -> AdvSim
finishCrossing sim = sim { advCrossing = []
                         , crossingSpeed = 0.0
                         , crossingDistance = 0
                         , crossingDirection = not (crossingDirection sim)
                         , currentLog = 1 + (currentLog sim)
                         , elapsedTime = (elapsedTime sim) + (fromIntegral crossingTime) }
    where
        crossingTime = maximum $ map (getTimeAdv . snd) $ filter (\(a,b) -> a `elem` (advCrossing sim)) $ zip [0..(pred . length $ adventurers)] (lefts adventurers)

updateCrossing :: Float -> AdvSim -> AdvSim
updateCrossing sec sim = sim { crossingDistance = (crossingDistance sim) + (crossingSpeed sim) * sec }
    
initCrossing :: AdvSim -> AdvSim
initCrossing sim = sim { advCrossing = crossingAdvs, crossingSpeed = cSpeed } 
    where
        cTrace = (!! (currentTrace sim)) . remLog . trace $ sim
        cLog = map ((fst cTrace) !! (currentLog sim)) adventurers
        nextLog = map ((fst cTrace) !! ((currentLog sim) + 1)) adventurers
        crossingAdvs = map fst $ filter (\(a,b) -> b) $ zip [0 .. (length adventurers) - 1] (zipWith (/=) cLog nextLog)
        ads = map (\(Left a) -> a) adventurers
        cDuration = maximum $ map (getTimeAdv . fst) $ filter (\(a,b) -> b) $ zip ads (zipWith (/=) cLog nextLog)
        cSpeed = bridgeSize / ((fromIntegral cDuration))          

moveAdvs :: Float -> AdvSim -> AdvSim
moveAdvs sec sim | not (started sim) = sim
                 | preCondition && (advCrossing sim == []) = initCrossing sim
                 | ((crossingDistance sim) >= bridgeSize) = finishCrossing sim
                 | (advCrossing sim) /= [] = updateCrossing sec sim
                 | otherwise = sim
    where
        preCondition = (length . remLog . trace $ sim) > 0 && (length . fst . (!! (currentTrace sim)) . remLog $ trace sim) > (currentLog sim) + 1


getTraces :: Int -> Int -> LogListDur State
getTraces steps maxTime = logManyChoice . map (logList . addLast) $ traces
    where
        traces = (filter (\(st,(t,s)) -> (t <= maxTime) && (s == gFinal)) . unpackLogs . logExec steps) gInit
        addLast (l,(t,d)) = (l ++ [d], (t,d))
        logList (l,(t,d)) = toLogListDur t l d

initialState :: (Int, Int) -> Picture -> Picture -> AdvSim
initialState (steps, maxTime) bridge lantern = Sim { bridgeImage = bridge
                                                , lanternImage = lantern
                                                , trace = getTraces steps maxTime
                                                , currentTrace = 0
                                                , currentLog = 0
                                                , crossingDirection = True
                                                , advCrossing = []
                                                , crossingSpeed = 0.0
                                                , crossingDistance = 0
                                                , elapsedTime = 0.0
                                                , args = (steps, maxTime)
                                                , started = False
                                                }

----------------------------------------------------------------------------------------

keyHandler :: Event -> AdvSim -> AdvSim
keyHandler (EventKey (Char 'l') _ _ _) sim = (initialState (args sim) (bridgeImage sim) (lanternImage sim)) { currentTrace = (flip mod (length . remLog . trace $ sim) . succ . currentTrace $ sim) }
keyHandler (EventKey (Char 'r') _ _ _) sim = (initialState (args sim) (bridgeImage sim) (lanternImage sim)) { currentTrace = (currentTrace sim)}
keyHandler (EventKey (Char 'k') _ _ _) sim = sim { crossingSpeed = 1.2 * (crossingSpeed sim) }
keyHandler (EventKey (Char 'j') _ _ _) sim = sim { crossingSpeed = (crossingSpeed sim) * 0.8 }
keyHandler (EventKey (Char 'i') _ _ _) sim = sim { started = True }
keyHandler _ sim = sim 

main :: IO ()
main = do
        Just bridge <- loadJuicyPNG "Bridge.png"
        Just lantern <- loadJuicyPNG "Lantern.png"
        [steps, maxTime] <- map (read :: String -> Int) <$> getArgs
        play  window background fr (initialState (steps, maxTime) bridge lantern) render keyHandler moveAdvs
