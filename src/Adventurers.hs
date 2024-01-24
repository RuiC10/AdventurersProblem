{-# LANGUAGE FlexibleInstances #-}
module Adventurers where
import DurationMonad
import Data.List ( subsequences )
import Data.Either ( isLeft, lefts )

-------------- utils

(f >< g) (a,b) = (f a, g b)
(f `split` g) x = (f x, g x)
------------------------

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show, Eq, Enum)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

objects :: [Objects]
objects = adventurers++[lantern]

adventurers :: [Either Adventurer b]
adventurers = Left <$> [P1 ..P10]

lantern :: Either a ()
lantern = Right ()

-- Maximum number of crossers
maxCrossers :: Int
maxCrossers = 2

-- The time that each adventurer needs to cross the bridge
-- To implement 
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}

type State = Objects -> Bool

instance Show State where
  show = show . (<$> objects)

instance Eq State where
  s1 == s2 = all (\o -> s1 o == s2 o) objects

-- The initial state of the game
gInit :: State
gInit = const False

-- The final state of the game
gFinal :: State
gFinal = const True

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os
       
{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice $ map (\l -> toListDur (getTime l) (mChangeState l s)) validPlays
    where validPlays = map (lantern:) $ filter preCondition $ subsequences adventurers 
          preCondition l = l /= [] && length l <= maxCrossers && all ((s lantern ==) . s) l 
          getTime = maximum . fmap getTimeAdv . lefts 
          
-- To implement 
exec :: Int -> State -> ListDur State
exec n = (!! n) . iterate (>>= allValidPlays) . return 

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = (any (\(t,s) -> (t <= 17) && (s == gFinal)) . unpack . exec 5) gInit

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = (any (\(t,s) -> (t < 17) && (s == gFinal)) . unpack . exec 5) gInit

--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

-- To implement
instance Functor ListDur where
   fmap = curry $ LD . uncurry fmap . (fmap >< remLD)

-- To implement
instance Applicative ListDur where
   pure = LD . pure . pure
   (<*>) = curry $ LD . uncurry (<*>) . ((fmap (<*>) . remLD) >< remLD)

-- To implement
instance Monad ListDur where
   return = LD . return . return
   (LD l) >>= k = LD [wait (getDuration x) y | x <- l, y <- remLD (k (getValue x))] 

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concatMap remLD

toListDur :: Int -> a -> ListDur a
toListDur n = LD . pure . wait n . pure 

unpack :: ListDur a -> [(Int,a)]
unpack = map (getDuration `split` getValue) . remLD 

--------------------------------------------------------------------------

data LogListDur a = LogLD [([State], Duration a)] deriving Show

remLog :: LogListDur a -> [([State], Duration a)]
remLog (LogLD x) = x

instance Functor LogListDur where
    fmap = curry $ LogLD . uncurry fmap . (((id >< ) . fmap) >< remLog)

-- Pointfree
instance Applicative LogListDur where
    pure = LogLD . pure . split (const []) pure
    (<*>) = curry $ LogLD . fmap (split (uncurry (++) . (fst >< fst)) (uncurry (<*>) . (snd >< snd))) . uncurry zip . (remLog >< remLog)

{- Pointwise
instance Applicative LogListDur where
    pure x = LogLD [([], pure x)] 
    l1 <*> l2 = LogLD [(x1 ++ y1, x2 <*> y2) | (x1, x2) <- remLog l1, (y1, y2) <- remLog l2]
-}

instance Monad LogListDur where
    return = LogLD . return . split (const []) return 
    l >>= k = LogLD [(logs ++ log', wait (getDuration dur) dur') | (logs, dur) <- remLog l, (log', dur') <- remLog (k (getValue dur))] 

toLogListDur :: Int -> [State] -> a -> LogListDur a
toLogListDur n = curry $ LogLD . pure . (id >< (wait n . pure))

logManyChoice :: [LogListDur a] -> LogListDur a
logManyChoice = LogLD . concatMap remLog

unpackLogs :: LogListDur a -> [([State], (Int,a))]
unpackLogs = map (id >< (getDuration `split` getValue)) . remLog
              
--------------------------------------------------------------------------

logAllValidPlays :: State -> LogListDur State
logAllValidPlays s = logManyChoice $ map(\l -> toLogListDur (getTime l) [s] (mChangeState l s)) validPlays
    where 
        validPlays = map (lantern:) $ filter preCondition $ subsequences adventurers 
        preCondition l = l /= [] && length l <= maxCrossers && all ((s lantern ==) . s) l 
        getTime = maximum . fmap getTimeAdv . lefts 

logExec :: Int -> State -> LogListDur State  
logExec n = (!! n) . iterate (>>= logAllValidPlays) . return

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}

-- To implement
leq17' :: Bool
leq17' = (any (\(st,(t,s)) -> (t <= 17) && (s == gFinal)) . unpackLogs . logExec 5) gInit

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}

-- To implement
l17' :: Bool
l17' = (any (\(st,(t,s)) -> (t < 17) && (s == gFinal)) . unpackLogs . logExec 5) gInit
