import System.IO
import Control.Applicative
import Data.Array
import Data.List
import Data.Fixed
import Lens.Micro

data Quadrant = UR | DR | DL | UL deriving (Ord, Eq, Show)

main :: IO ()
main = sol2

sol1 :: IO ()
sol1 = do
    input <- lines <$> readFile "inp10.txt"
    print $ getMaxAsteroids $ toAsteroidArray input

sol2 :: IO ()
sol2 = do
    input <- lines <$> readFile "inp10.txt"
    let watchSpot = snd $ getMaxAsteroids $ toAsteroidArray input
    let indexed = [fst x | x <- indexLst input, snd x == '#' && fst x /= watchSpot]
    let sorted = sortBy compareRotation $ map (vectorize watchSpot) indexed
    print sorted
    print $ map (deVectorize watchSpot . fst) $ take 199 $ iterate (\(x, xs) -> vaporizeAsteroid xs x) (head sorted, tail sorted)

indexLst :: [[a]] -> [((Int, Int), a)]
indexLst lst = concat indexedLst
        where 
            indexedYs = zip [0..] lst
            indexedLst = map (\(yIx, x) -> zip (zip [0..] (repeat yIx)) x) indexedYs


toAsteroidArray :: [String] -> Array (Int, Int) Char
toAsteroidArray lst = array ((0,0), (lenX,lenY)) $ indexLst lst
    where
        lenX = length (head lst) - 1
        lenY = length lst - 1
                                  
getMaxAsteroids :: Array (Int, Int) Char -> (Int, (Int, Int))
getMaxAsteroids arr = maximum [(getCount arr (x,y) - 1, (x,y)) | (x,y) <- indices arr, arr!(x,y) == '#']
    where (_, (maxX, maxY)) = bounds arr

getCount :: Array (Int, Int) Char -> (Int, Int) -> Int
getCount arr from = foldl (\i v -> if v then i + 1 else i) 0
                          [isReachable arr from x | x <- indices arr, arr!x == '#']

isReachable :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool
isReachable arr from to = 
    let step = takeStep from to
    in (from == to) || (step == to) ||
       (arr!step == '.' && isReachable arr step to)

takeStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
takeStep from@(fX,fY) to@(tX, tY) =
    case (tX-fX, tY-fY) of
      (0, y) -> if y > 0 then (fX, fY+1) else (fX, fY-1)
      (x, 0) -> if x > 0 then (fX+1, fY) else (fX-1, fY)
      (x, y) -> let gcd' = gcd x y in (fX + x `div` gcd', fY + y `div` gcd')

vaporizeAsteroid :: [(Int, Int)] -> (Int, Int) -> ((Int, Int), [(Int, Int)])
vaporizeAsteroid [x] _ = (x, [])
vaporizeAsteroid (x:xs) prev = if takeStep (0,0) x == takeStep (0,0) prev
                                  then vaporizeAsteroid (xs ++ [x]) prev
                                  else (x, xs)

vectorize :: (Int, Int) -> (Int, Int) -> (Int, Int)
vectorize (xFrom, yFrom) (xTo, yTo) = (xTo - xFrom, - (yTo - yFrom))

deVectorize :: (Int, Int) -> (Int, Int) -> (Int, Int) 
deVectorize (xFrom, yFrom) (xTo, yTo) = (xTo + xFrom, yFrom - yTo)

getQuadrant :: (Int, Int) -> Quadrant
getQuadrant (x,y)
  | x >= 0 && y >= 0 = UR
  | x >= 0 && y < 0 = DR
  | x < 0 && y < 0 = DL
  | otherwise = UL

angle :: (Int, Int) -> Double
angle (x, y) = let arctan = atan2 (fromIntegral x) (fromIntegral y)
                in if arctan > 0 then arctan else 2*pi + arctan

compareRotation :: (Int, Int) -> (Int, Int) -> Ordering
compareRotation a b =
    let
        stepA = takeStep (0,0) a
        stepB = takeStep (0,0) b
        quadA = getQuadrant stepA
        quadB = getQuadrant stepB
     in
        if stepA == stepB then b `compare` a
        else angle a `compare` angle b

renderImage :: [(Integer, Integer)] -> [String]
renderImage xs = [[if (x, y) `elem` xs then '#' else '.' | x <- [0..40]] | y <- reverse [-5..0]] 
