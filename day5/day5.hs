{-# LANGUAGE ViewPatterns #-}
import System.IO
import Data.List.Split (wordsBy)
import Data.Sequence (Seq(..), ViewL(..), index, update, viewl)
import qualified Data.Sequence as S
import Control.Monad.Trans.Writer
import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe, catMaybes)
import Data.List

data Opcode = 
      Add  Int Int Int
    | Mult Int Int Int
    | Inp  Int
    | Out  Int
    | Halt deriving (Show, Eq)

data Mode = Position | Immediate deriving (Show, Eq)
type Instruction = ([Mode], Opcode)

main :: IO ()
main = sol1

sol1 :: IO ()
sol1 = do
    prog <- parseIntcode <$> readFile "inp5.txt"
    print $ showTrace prog

parseIntcode :: String -> Seq Int
parseIntcode = S.fromList . map read . wordsBy (==',')

parseModes :: Int -> [Mode]
parseModes 0 = repeat Position
parseModes n = case n `mod` 10 of
                 1 -> Immediate : parseModes (n `div` 10)
                 0 -> Position : parseModes (n `div` 10)

parseOpcode :: Seq Int -> Opcode
parseOpcode (viewl -> i:<xs) = case i `mod` 100 of
    1 -> Add a b pos
        where a :<| b :<| pos :<| _ = xs
    2 -> Mult a b pos
        where a :<| b :<| pos :<| _ = xs
    3 -> Inp pos
        where pos :<| _ = xs
    4 -> Out pos
        where pos :<| _ = xs
    99 -> Halt

parseInstruction :: Seq Int -> Int -> Instruction
parseInstruction xs pc = 
    let 
        xs' = S.drop pc xs
        i :<| _ = xs
     in (parseModes (i `div` 100), parseOpcode xs')

intcodeStep :: Seq Int -> Int -> Writer [String] (Seq Int, Int)
intcodeStep prog pc = 
    let (modes, op) = parseInstruction prog pc
    in writer $ case op of
         Add a b pos -> ((update pos (a'+b') prog, pc + 4), [])
             where
                 a' = if head modes == Immediate 
                         then a else prog `index` a
                 b' = if head (tail modes) == Immediate 
                         then b else prog `index` b
         Mult a b pos -> ((update pos (a'*b') prog, pc + 4), [])
             where
                 a' = if head modes == Immediate 
                         then a else prog `index` a
                 b' = if head (tail modes) == Immediate 
                         then b else prog `index` b
         Inp pos -> ((update pos 1 prog, pc + 2), [])
         Out pos -> ((prog, pc + 2), [show $ prog `index` pos'])
             where pos' = if head modes == Immediate 
                             then pos else prog `index` pos
         Halt -> ((S.Empty, 0), [])

runIntcode :: Seq Int -> Writer [String] (Seq Int, Int)
runIntcode xs = fromMaybe (writer ((S.Empty, 0), [])) $ find 
    (\x -> (fst . fst) (runWriter x) == S.Empty) 
    (iterate (>>= uncurry intcodeStep) (writer ((xs, 0), [])))

showTrace :: Seq Int -> [Writer [String] (Seq Int, Int)]
showTrace xs = takeWhile 
    (\x -> (fst . fst) (runWriter x) /= S.Empty) 
    (iterate (>>= uncurry intcodeStep) (writer ((xs, 0), [])))

testProgram :: Seq Int
testProgram = S.fromList [1002,4,3,4,33]
