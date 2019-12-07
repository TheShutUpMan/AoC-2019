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
    | JmpT Int Int
    | JmpF Int Int
    | LTi   Int Int Int
    | EQi   Int Int Int
    | Halt deriving (Show, Eq)

data Mode = Position | Immediate deriving (Show, Eq)
type Instruction = ([Mode], Opcode)

main :: IO ()
main = sol1

sol1 :: IO ()
sol1 = do
    prog <- parseIntcode <$> readFile "inp5.txt"
    print $ showTrace prog -- set testInput to 1

sol2 :: IO ()
sol2 = sol1 -- set testInput to 5

testInput :: Int
testInput = 5

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
    5 -> JmpT bool pos
        where bool :<| pos :<| _ = xs
    6 -> JmpF bool pos
        where bool :<| pos :<| _ = xs
    7 -> LTi a b pos
        where a :<| b :<| pos :<| _ = xs
    8 -> EQi a b pos
        where a :<| b :<| pos :<| _ = xs
    99 -> Halt

parseInstruction :: Seq Int -> Int -> Instruction
parseInstruction xs pc = 
    let 
        xs' = S.drop pc xs
        i :<| _ = xs'
     in (parseModes (i `div` 100), parseOpcode xs')

getValue :: Seq Int -> Mode -> Int -> Int
getValue prog mode val
  | mode == Immediate = val
  | otherwise = prog `index` val

intcodeStep :: Seq Int -> Int -> Writer [String] (Seq Int, Int)
intcodeStep prog pc = 
    let (modes, op) = parseInstruction prog pc
    in writer $ case op of
         Add a b pos -> ((update pos (a'+b') prog, pc + 4), [])
             where
                 a' = getValue prog (head modes) a
                 b' = getValue prog (head $ tail modes) b
         Mult a b pos -> ((update pos (a'*b') prog, pc + 4), [])
             where
                 a' = getValue prog (head modes) a
                 b' = getValue prog (head $ tail modes) b
         Inp pos -> ((update pos testInput prog, pc + 2), [])
         Out pos -> ((prog, pc + 2), [show $ getValue prog (head modes) pos])
         JmpT bool pos -> ((prog, pc'), [])
             where
                 pc' = if getValue prog (head modes) bool /= 0
                       then getValue prog (head $ tail modes) pos
                       else pc + 3
         JmpF bool pos -> ((prog, pc'), [])
             where
                 pc' = if getValue prog (head modes) bool == 0
                       then getValue prog (head $ tail modes) pos
                       else pc + 3
         LTi a b pos -> ((update pos newval prog, pc + 4), [])
             where
                 a' = getValue prog (head modes) a
                 b' = getValue prog (head $ tail modes) b
                 newval = if a' < b' then 1 else 0
         EQi a b pos -> ((update pos newval prog, pc + 4), [])
             where
                 a' = getValue prog (head modes) a
                 b' = getValue prog (head $ tail modes) b
                 newval = if a' == b' then 1 else 0
         Halt -> ((S.Empty, 0), [])

runIntcode :: Seq Int -> Writer [String] (Seq Int, Int)
runIntcode xs = fromMaybe (writer ((S.Empty, 0), [])) $ find 
    (\x -> (fst . fst) (runWriter x) == S.Empty) 
    (iterate (>>= uncurry intcodeStep) (writer ((xs, 0), [])))

showTrace :: Seq Int -> [Writer [String] (Seq Int, Int)]
showTrace xs = takeWhile 
    (\x -> (fst . fst) (runWriter x) /= S.Empty) 
    (iterate (>>= uncurry intcodeStep) (writer ((xs, 0), [])))

