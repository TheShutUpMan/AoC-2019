import System.IO
import Data.List.Split (wordsBy)
import Data.Sequence (Seq, index, update)
import qualified Data.Sequence as S

main :: IO ()
main = sol2

sol1 :: IO ()
sol1 = do
    input <- readFile "inp2.txt"
    let inpProg = S.fromList $ map read $ wordsBy (==',') input
    let prog = addInputs inpProg (12, 2)
    print $ show $ runIntcode prog 0

sol2 = do
    input <- readFile "inp2.txt"
    let inpProg = S.fromList $ map read $ wordsBy (==',') input
    let inputs = [(i,j) | i <- [1..S.length inpProg], j <- [1.. S.length inpProg]]
    print $ show $ tryAllInputs inputs inpProg
    

tryAllInputs :: [(Int, Int)] -> Seq Int -> Maybe (Int, Int)
tryAllInputs [] _ = Nothing
tryAllInputs (x:xs) s = if (runIntcode (addInputs s x) 0) == 19690720
                        then Just x
                        else tryAllInputs xs s


addInputs :: Seq Int -> (Int, Int) -> Seq Int
addInputs prog (noun, verb) = update 1 noun $ update 2 verb prog

runIntcode :: Seq Int -> Int -> Int
runIntcode prog pc = if (prog `index` pc == 99)
                     then prog `index` 0
                     else runIntcode (intcodeStep prog pc) (pc + 4)

intcodeStep :: Seq Int -> Int -> Seq Int
intcodeStep prog pc = case (prog `index` pc) of
                        1 -> update (prog `index` (pc + 3))
                                       (prog `index` (prog `index` (pc + 1)) + 
                                        prog `index` (prog `index` (pc + 2)))
                                    prog
                        2 -> update (prog `index` (pc + 3))
                                       (prog `index` (prog `index` (pc + 1)) *
                                        prog `index` (prog `index` (pc + 2)))
                                    prog
