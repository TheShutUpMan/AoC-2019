import System.IO
import Data.List.Split (wordsBy)
import Data.List (foldl')

main :: IO ()
main = sol2

sol1 :: IO ()
sol1 = do
    input <- readFile "inp4.txt"
    let nums = map read (wordsBy (=='-') input) :: [Int]
    print $ show $ foldl' (flip ((+) . fromEnum)) 0 $ map validPass [head nums .. last nums]

sol2 :: IO ()
sol2 = do
    input <- readFile "inp4.txt"
    let nums = map read (wordsBy (=='-') input) :: [Int]
    print $ show $ foldl' (flip ((+) . fromEnum)) 0 $ map validPass2 [head nums .. last nums]

validPass :: Int -> Bool
validPass n = 
    let numsList = map (read . (:"")) (show n) :: [Int]
     in (fst $ foldl' (\x y -> (fst x && snd x <= y, y)) (True, 0) numsList) &&
         (fst $ foldl' (\x y -> (fst x || snd x == y, y)) (False, 0) numsList)

validPass2 :: Int -> Bool
validPass2 n =
    let numsList = map (read . (:"")) (show n) :: [Int]
     in (fst $ foldl' (\x y -> (fst x && snd x <= y, y)) (True, 0) numsList) &&
        (fst $ foldl' (\x y -> (fst x || snd x == y, y)) (False, 0) numsList) &&
        (groupOf2 numsList || isStart2 numsList)

isStart2 :: [Int] -> Bool
isStart2 (x:y:z:_) = x == y && y /= z

groupOf2 :: [Int] -> Bool
groupOf2 (w:x:y:z:[]) = (x /= y && y == z) || (w /= x && x == y && y /= z)
groupOf2 (w:xs@(x:y:z:ys)) = (w /= x && x == y && y /= z) || groupOf2 xs
