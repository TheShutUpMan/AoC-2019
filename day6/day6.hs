{-# LANGUAGE TupleSections #-}
import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Tree
import Data.List.Split (splitOn)
import Data.Map.Strict (fromList, toList, fromListWith, (!?), (!), Map)
import Data.Tuple (swap)

main :: IO ()
main = sol2

sol1 :: IO ()
sol1 = do
    pairs <- toPairs <$> readFile "inp6.txt"
    let orbits = fromListWith (++) pairs
    print $ countOrbits orbits (getRoot pairs)

sol2 = do
    pairs <- toPairs <$> readFile "inp6.txt"
    let orbits = fromListWith (++) pairs
    print $ (getDistance orbits "SAN" "YOU") - 2

toPairs :: String -> [(String, [String])]
toPairs = map (toTuple . splitOn ")") . lines
    where toTuple [x,y] = (x,[y])

getRoot :: [(String, [String])] -> String
getRoot xs = 
    let 
        (k,v) = unzip xs
        v' = concat v
    in
        head $ filter (not . flip elem v') k 

getDistance :: Map String [String] -> String -> String -> Int
getDistance orbits from to = dist r from + dist r to
    where 
        r = getCommonRoot orbits from to
        rev = reverseMap orbits
        dist r n = if r == n then 0 else 1 + dist r (rev ! n)

reverseMap :: Map String [String] -> Map String String
reverseMap m = fromList $ concatTups $ toList m
    where
        concatTups [] = []
        concatTups ((x,y):xs) = map (swap . (x,)) y ++ concatTups xs

getCommonRoot :: Map String [String] -> String -> String -> String
getCommonRoot orbits from to
  | isParent from to = from
  | isParent to from = to
  | otherwise = getCommonRoot orbits (rev ! from) (rev ! to)
    where
        rev = reverseMap orbits
        isParent x y = case rev !? y of
                         Nothing -> False
                         Just s -> if s == x then True else isParent x s


countOrbits :: Map String [String] -> String -> Int
countOrbits orbits root = helper 1 root
    where 
        helper n node = case orbits !? node of
                        Nothing -> 0
                        Just node -> sum $ map ((+n) . helper (n+1)) node

