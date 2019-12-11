import System.IO
import Control.Applicative
import Data.Array
import Data.List
import Lens.Micro

data Quadrant = UR | DR | DL | UL deriving (Ord, Eq, Show)

main :: IO ()
main = sol2

test :: IO [String]
test = lines <$> readFile "test.txt"

sol1 :: IO ()
sol1 = do
    input <- lines <$> readFile "test.txt"
    print $ getMaxAsteroids $ toAsteroidArray input

sol2 :: IO ()
sol2 = do
    input <- lines <$> readFile "test.txt"
    let watchSpot = snd $ getMaxAsteroids $ toAsteroidArray input
    let indexed = [fst x | x <- indexLst input, snd x == '#']
    let sorted = sortBy compareRotation $ map (vectorize watchSpot) indexed
    print $ map fst $ take 27 $ iterate (\(x, xs) -> vaporizeAsteroid xs x) ((1,0), sorted)

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
vaporizeAsteroid (x:xs) prev = if compareRotation x prev == EQ
                                  then vaporizeAsteroid (xs ++ [x]) prev
                                  else (x, xs)

vectorize :: (Int, Int) -> (Int, Int) -> (Int, Int)
vectorize (xFrom, yFrom) (xTo, yTo) = (xFrom - xTo, yFrom - yTo)

getQuadrant :: (Int, Int) -> Quadrant
getQuadrant (x,y)
  | x >= 0 && y >= 0 = UR
  | x >= 0 && y < 0 = DR
  | x < 0 && y < 0 = DL
  | otherwise = UL

cosV :: (Int, Int) -> Double
cosV (x, y) = fromIntegral y / sqrt (fromIntegral $ x^2 + y^2)

compareRotation :: (Int, Int) -> (Int, Int) -> Ordering
compareRotation a b =
    let
        stepA = takeStep (0,0) a
        stepB = takeStep (0,0) b
        quadA = getQuadrant stepA
        quadB = getQuadrant stepB
     in
        if stepA == stepB then EQ
        else case quadA `compare` quadB of
               EQ -> compareInQuadrant quadA stepA stepB
               c -> c
    where
        compareInQuadrant quadA a b =
            let cosA = cosV a
                cosB = cosV b
            in case quadA of
              UR -> cosA `compare` cosB
              DR -> (- cosA) `compare` (- cosB)
              DL -> (- cosA) `compare` (- cosB)
              UL -> cosA `compare` cosB
