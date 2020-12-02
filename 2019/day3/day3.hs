import qualified Data.Map.Strict as Map
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
    print $ show $ minimum $ map toManhattan $ findIntersections 
        (getPath $ head inpList)
        (toPositionMap $ getPath $ last inpList)

sol2 :: IO ()
sol2 = do
    input <- readFile "inp3.txt"
    let inpList = map ((map toCoord) . wordsBy (==',')) $ lines input
    print $ show $ minimum $ findStepIntersections 
        (zip (getPath $ head inpList) [1..])
        (toPositionStepMap $ getPath $ last inpList)

toCoord :: String -> Coord
toCoord s = (read ([head s]), read $ tail s)

coordStep :: Coord -> (Int, Int) -> [(Int, Int)]
coordStep (d, n) (x, y) = case d of
    U -> [(x,y+n') | n' <- [1..n]]
    D -> [(x,y-n') | n' <- [1..n]]
    R -> [(x+n',y) | n' <- [1..n]]
    L -> [(x-n',y) | n' <- [1..n]]

getPath :: [Coord] -> [(Int, Int)]
getPath cs = helper cs (0,0)
    where
        helper :: [Coord] -> (Int, Int) -> [(Int, Int)]
        helper [] p = []
        helper (c:cs) p =
            let step = coordStep c p
            in step ++ (helper cs (last step))

toPositionMap :: [(Int, Int)] -> Map.Map (Int, Int) ()
toPositionMap = Map.fromList . (map (\x -> (x,())))

toPositionStepMap :: [(Int, Int)] -> Map.Map (Int,Int) Int
toPositionStepMap = Map.fromList . flip zip [1..]

findIntersections :: [(Int, Int)] -> Map.Map (Int, Int) () -> [(Int, Int)]
findIntersections [] _ = []
findIntersections (x:xs) map = case Map.lookup x map of
                                 Nothing -> findIntersections xs map
                                 Just () -> x : findIntersections xs map

findStepIntersections :: [((Int, Int), Int)] -> Map.Map (Int, Int) Int -> [Int]
findStepIntersections [] _ = []
findStepIntersections (x:xs) map = case Map.lookup (fst x) map of
                                 Nothing -> findStepIntersections xs map
                                 Just n -> (n + snd x) : findStepIntersections xs map

toManhattan :: (Int, Int) -> Int
toManhattan (x,y) = (abs x) + (abs y)
