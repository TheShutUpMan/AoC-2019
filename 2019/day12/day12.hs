{-# LANGUAGE TemplateHaskell #-}
import System.IO
import Control.Applicative
import Lens.Micro
import Lens.Micro.TH
import qualified Data.Set as S

data V3 a = V3
    { _x :: a
    , _y :: a
    , _z :: a
    } deriving (Show, Eq)

instance Functor V3 where
    fmap f v = V3 (f $ _x v) (f $ _y v) (f $ _z v)

instance Applicative V3 where
    pure x = V3 x x x
    f <*> v = V3 (_x f $ _x v) (_y f $ _y v) (_z f $ _z v) 

data Moon = Moon
    { _pos :: V3 Int
    , _vel :: V3 Int
    } deriving (Show, Eq)

makeLenses ''V3
makeLenses ''Moon

moons :: [Moon]
moons = map (uncurry Moon) [
    (V3 9 13 (-8), V3 0 0 0),
    (V3 (-3) 16 (-17), V3 0 0 0),
    (V3 (-4) 11 (-10), V3 0 0 0),
    (V3 0 (-2) (-2), V3 0 0 0)
    ]

main :: IO ()
main = sol2

sol1 :: IO ()
sol1 = print $ getEnergy $ iterate timeStep moons !! 1000 

sol2 :: IO ()
sol2 = print $ foldr (lcm . findFix timeStepAxis . getAxis moons) 1 [_x, _y, _z]
    
getEnergy :: [Moon] -> Int
getEnergy [] = 0
getEnergy (x:xs) = let (p, v) = (_pos x, _vel x)
                    in (abs (_x p) + abs (_y p) + abs (_z p)) *
                       (abs (_x v) + abs (_y v) + abs (_z v)) + getEnergy xs

getAxis :: [Moon] -> (V3 Int -> Int) -> [(Int, Int)]
getAxis xs f = let tups = map (\x -> (_pos x, _vel x)) xs
                in map (\(p, v) -> (f p, f v)) tups

timeStep :: [Moon] -> [Moon]
timeStep = updatePosition . applyGravity

timeStepAxis :: [(Int, Int)] -> [(Int, Int)]
timeStepAxis = updateAx . gravityAx
    where
        updateAx = map (\(x,y) -> (x+y, y))
        gravityAx xs = [attractAx xs x | x <- xs]

attractAx :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
attractAx [] x = x
attractAx ((p', _):xs) (p,v) = (p, if p > p' then v-1
                                else if p < p' then v+1
                                else v) & attractAx xs

applyGravity :: [Moon] -> [Moon]
applyGravity xs = [attract x xs | x <- xs]

attract :: Moon -> [Moon] -> Moon
attract moon [] = moon
attract moon (x:xs) =
    let (v, p) = (_vel moon, _pos moon)
     in attract (Moon p ((\p v x -> if p > x then v - 1
                                else if p < x then v + 1 else v) <$> p <*> v <*> _pos x)) xs

updatePosition :: [Moon] -> [Moon]
updatePosition = map (\x -> x & pos %~ liftA2 (+) (_vel x))

findFix :: (Eq a, Ord a)=> (a -> a) -> a -> Int
findFix f x = helper f x S.empty
    where helper f x xs = if x `S.member` xs
                        then 0 else 1 + helper f (f x) (S.insert x xs)

testMoons :: [Moon]
testMoons = map (uncurry Moon) [
    (V3 (-1) 0 2, V3 0 0 0),
    (V3 2 (-10) (-7), V3 0 0 0),
    (V3 4 (-8) 8, V3 0 0 0),
    (V3 3 5 (-1), V3 0 0 0)
    ]
