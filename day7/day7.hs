{-# LANGUAGE ViewPatterns #-}
import System.IO
import Data.List.Split (wordsBy)
import Data.Sequence (Seq(..), ViewL(..), index, update, viewl)
import qualified Data.Sequence as S
import Control.Monad.Trans.Writer
import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Tuple.Extra
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
data ProgState = ProgState
    { _inputs  :: [Int]
    , _program :: Seq Int
    , _pc      :: Int
    } deriving (Show)
    

main :: IO ()
main = sol1

sol1 :: IO ()
sol1 = do
    prog <- parseIntcode <$> readFile "inp7.txt"
    print $ maximum $ map (runAllThrusters prog) (permutations [0..4])

{- sol2 = do
    prog <- parseIntcode <$> readFile "inp7.txt"
    print $ maximum $ map (thrusterLoop prog) (permutations [5..9]) -}

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

intcodeStep :: ProgState -> Writer [Int] ProgState
intcodeStep state = 
    let 
        prog = _program state
        pc = _pc state
        inputs = _inputs state
        (modes, op) = parseInstruction prog pc
    in writer $ case op of
         Add a b pos -> (ProgState inputs (update pos (a'+b') prog) (pc + 4), [])
             where
                 a' = getValue prog (head modes) a
                 b' = getValue prog (head $ tail modes) b
         Mult a b pos -> (ProgState inputs (update pos (a'*b') prog) (pc + 4), [])
             where
                 a' = getValue prog (head modes) a
                 b' = getValue prog (head $ tail modes) b
         Inp pos -> (ProgState rest (update pos input prog) (pc + 2), [])
             where 
                 input = head inputs
                 rest = tail inputs
         Out pos -> (ProgState inputs prog (pc + 2), [getValue prog (head modes) pos])
         JmpT bool pos -> (ProgState inputs prog pc', [])
             where
                 pc' = if getValue prog (head modes) bool /= 0
                       then getValue prog (head $ tail modes) pos
                       else pc + 3
         JmpF bool pos -> (ProgState inputs prog pc', [])
             where
                 pc' = if getValue prog (head modes) bool == 0
                       then getValue prog (head $ tail modes) pos
                       else pc + 3
         LTi a b pos -> (ProgState inputs (update pos newval prog) (pc + 4), [])
             where
                 a' = getValue prog (head modes) a
                 b' = getValue prog (head $ tail modes) b
                 newval = if a' < b' then 1 else 0
         EQi a b pos -> (ProgState inputs (update pos newval prog) (pc + 4), [])
             where
                 a' = getValue prog (head modes) a
                 b' = getValue prog (head $ tail modes) b
                 newval = if a' == b' then 1 else 0
         Halt -> (ProgState inputs S.Empty 0, [])

runIntcode :: Seq Int -> [Int] -> Writer [Int] ProgState
runIntcode xs inputs = fromMaybe (writer (ProgState inputs S.Empty 0, [])) $ find 
    (\x -> (_program . fst) (runWriter x) == S.Empty) 
    (iterate (>>= intcodeStep) (writer (ProgState inputs xs 0, [])))

getOutput :: Writer [Int] a -> Int
getOutput state = let (_, x:[]) = runWriter state in x

runAllThrusters :: Seq Int -> [Int] -> Int
runAllThrusters xs inputs = helper xs inputs 0
    where helper xs [] val = val
          helper xs (i:inps) val = helper xs inps (getOutput (runIntcode xs [i,val]))

runUntilOutput :: ProgState -> [Int] -> Maybe Writer [Int] ProgState
runUntilOutput state inputs = find 
    (\x -> (snd . runWriter) x /= [])
    (iterate (>>= intcodeStep) (writer (state, [])))

thrusterLoop :: Seq Int -> [Int] -> [Int]
thrusterLoop = undefined 




