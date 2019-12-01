import System.IO
import Control.Monad

main :: IO ()
main = sol2

getFuel :: Int -> Int
getFuel = (subtract 2) . (flip div) 3

getTotalFuel :: Int -> Int
getTotalFuel f = foldr (+) 0 $ takeWhile (>0) (tail $ iterate getFuel f)

sol1 :: IO ()
sol1 = do
    input <- readFile "inp1.txt"
    let numbers = map read $ lines input
    let sum_ = foldr (+) 0 (map getFuel numbers)
    print $ show sum_

sol2 :: IO ()
sol2 = do
    input <- readFile "inp1.txt"
    let numbers = map read $ lines input
    let sum_ = foldr (+) 0 (map getTotalFuel numbers)
    print $ show sum_
