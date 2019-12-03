import Data.Array
import Data.List.Split (wordsBy)
import System.IO

data Dir = U | D | L | R deriving (Eq, Show, Read)
type Coord = (Dir, Int)

main :: IO ()
main = sol1

sol1 :: IO ()
sol1 = do
    input <- readFile "inp3.txt"
    let inpList = map ((map toCoord) . wordsBy (==',')) $ lines input
    print $ show $ accumArray (||) False (getMaxArrayBounds $ head inpList) []
    print $ show $ map getWireArray inpList

toCoord :: String -> Coord
toCoord s = (read ([head s]), read $ tail s)

getMaxArrayBounds :: [Coord] -> ((Int, Int), (Int, Int))
getMaxArrayBounds xs = (
    (negate $ sum $ map snd $ filter ((==L) . fst) xs,
    sum $ map snd $ filter ((==R) . fst) xs),
    (negate $ sum $ map snd $ filter ((==D) . fst) xs,
    sum $ map snd $ filter ((==U) . fst) xs))

getWireArray :: [Coord] -> Array (Int, Int) Bool
getWireArray xs = helper xs (0,0) $ accumArray (||) False (getMaxArrayBounds xs) []
    where
        helper [] _ arr = arr
        helper (c:cs) (x,y) arr = case fst c of
            U -> helper cs (x, y+snd c) (arr // [((x, y+n), True) | n <- [0..snd c]])
            D -> helper cs (x, y-snd c) (arr // [((x, y-n), True) | n <- [0..snd c]])
            L -> helper cs (x-snd c, y) (arr // [((x-n, y), True) | n <- [0..snd c]])
            R -> helper cs (x+snd c, y) (arr // [((x+n, y), True) | n <- [0..snd c]])

