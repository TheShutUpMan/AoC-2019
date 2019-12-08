import System.IO (readFile)
import Data.List

main :: IO ()
main = sol2

sol1 :: IO ()
sol1 = do
    input <- fmap (filter (/= '\n')) $ readFile "inp8.txt" 
    let annotatedLayers = map (\x -> (count '0' x, x)) (toLayers (25,6) input)
    let (_, maxLayer) = foldl' (\x@(n,_) y@(m,_) -> if n<m then x else y) (10000, []) annotatedLayers
    print $ (count '1' maxLayer) * (count '2' maxLayer)


sol2 :: IO ()
sol2 = do
    input <- fmap (filter (/= '\n')) $ readFile "inp8.txt" 
    let decodedLayers = decodeImage $ toLayers (25,6) input
    putStr $ showImage 25 decodedLayers


toLayers :: (Int, Int) -> String -> [String]
toLayers _ [] = []
toLayers (x,y) s = take (x*y) s : (toLayers (x,y) $ drop (x*y) s)

count :: Eq a => a -> [a] -> Int
count n = length . filter (==n)

decodeImage :: [String] -> String
decodeImage = map getPixel . transpose

getPixel :: String -> Char
getPixel (x:xs)
  | x == '2' = getPixel xs
  | otherwise = x

showImage :: Int -> String -> String
showImage _ [] = []
showImage 1 (x:xs) = x:'\n':(showImage 25 xs)
showImage n (x:xs) = x : (showImage (n-1) xs)
