{-# LANGUAGE ViewPatterns #-}
import System.IO
import Data.List.Split (wordsBy)
import Data.Sequence (Seq(..), ViewL(..), index, update, viewl)
import qualified Data.Sequence as S

data Instruction = 
      Add  Int Int Int
    | Mult Int Int Int
    | Inp  Int
    | Out  Int
    | Halt deriving (Show)

data Mode = Position | Immediate deriving (Show)

parseIntcode :: String -> Seq Int
parseIntcode = S.fromList . map read . wordsBy (==',')

parseInstruction :: Seq Int -> Maybe (Instruction, Seq Int)
parseInstruction (viewl -> EmptyL) = Nothing
parseInstruction (viewl -> i :< xs) = Just $ case i `mod` 100 of
    1 -> (Add a b pos, rest)
        where a :<| b :<| pos :<| rest = xs
    2 -> (Mult a b pos, rest)
        where a :<| b :<| pos :<| rest = xs
    3 -> (Inp pos, rest)
        where pos :<| rest = xs
    4 -> (Out pos, rest)
        where pos :<| rest = xs

intcodeStep :: Seq Int -> Int -> (Int, Seq Int)
intcodeStep prog pc = case (prog `index` pc) of
                        1 -> (4, update (prog `index` (pc + 3))
                                       (prog `index` (prog `index` (pc + 1)) + 
                                        prog `index` (prog `index` (pc + 2)))
                                    prog)
                        2 -> (4, update (prog `index` (pc + 3))
                                       (prog `index` (prog `index` (pc + 1)) *
                                        prog `index` (prog `index` (pc + 2)))
                                    prog)

runIntcode :: Seq Int -> Int -> Int
runIntcode prog pc = if (prog `index` pc == 99)
                     then prog `index` 0
                     else runIntcode nextStep nextPc
                         where (nextPc, nextStep) = intcodeStep prog pc
