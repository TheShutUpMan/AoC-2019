{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Intcode where

import System.IO
import Data.List.Split (wordsBy)
import Data.Sequence (Seq(..), ViewL(..), index, update, viewl, (!?), (><))
import qualified Data.Sequence as S
import Control.Monad.Trans.Writer
import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra
import Data.List
import Lens.Micro.TH
import Lens.Micro

data Opcode = 
      Add  Integer Integer Integer
    | Mult Integer Integer Integer
    | Inp  Integer
    | Out  Integer
    | JmpT Integer Integer
    | JmpF Integer Integer
    | LTi   Integer Integer Integer 
    | EQi   Integer Integer Integer
    | RelA  Integer
    | Halt deriving (Show, Eq)

data Mode = Position | Relative | Immediate deriving (Show, Eq)
type Instruction = ([Mode], Opcode)
data ProgState = ProgState
    { _inps    :: [Integer]
    , _program :: Seq Integer
    , _pc      :: Integer
    , _rel     :: Integer 
    } deriving (Show)

makeLenses ''ProgState

parseIntcode :: String -> Seq Integer
parseIntcode = S.fromList . map read . wordsBy (==',')

parseModes :: Integer -> [Mode]
parseModes 0 = repeat Position
parseModes n = case n `mod` 10 of
                 0 -> Position : parseModes (n `div` 10)
                 1 -> Immediate : parseModes (n `div` 10)
                 2 -> Relative  : parseModes (n `div` 10)

parseOpcode :: Seq Integer -> Opcode
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
    9 -> RelA update
        where update :<| _ = xs
    99 -> Halt

parseInstruction :: Seq Integer -> Integer -> Instruction
parseInstruction xs pc = 
    let 
        xs' = S.drop (fromInteger pc) xs
        i :<| _ = xs'
     in (parseModes (i `div` 100), parseOpcode xs')

getValue :: ProgState -> Mode -> Integer -> Integer
getValue state mode val
  | mode == Immediate = val
  | mode == Relative = fromMaybe 0 $
      (state ^. program) !? fromInteger (val + (state ^. rel))
  | otherwise = fromMaybe 0 $ (state ^. program) !? fromInteger val

setValue :: Integer -> Integer -> Mode -> ProgState -> ProgState
setValue index value mode state =
    let len = S.length (state ^. program)
        ix = if mode == Relative
             then fromInteger (index + state ^. rel)
             else fromInteger index
    in if len >= ix then state & program %~ update ix value
                    else setValue index value mode (state & program %~
                                       (>< S.replicate len 0))

updatePC :: Integer -> ProgState -> ProgState
updatePC i st = st & pc +~ i

setPC :: Integer -> ProgState -> ProgState
setPC i st = st & pc .~ i

intcodeStep :: ProgState -> Writer [Integer] ProgState
intcodeStep state = 
    let 
        prog = _program state
        pc = _pc state
        inputs = _inps state
        (modes, op) = parseInstruction prog pc
    in writer $ case op of
         Add a b pos -> (updatePC 4 (setValue pos (a'+b') (modes!!2) state), [])
             where
                 a' = getValue state (head modes) a
                 b' = getValue state (modes!!1) b
         Mult a b pos -> (updatePC 4 (setValue pos (a'*b') (modes !! 2) state), [])
             where
                 a' = getValue state (head modes) a
                 b' = getValue state (head $ tail modes) b
         Inp pos -> (updatePC 2 $ setValue pos input (head modes)
                                            (state & inps .~ rest), [])
             where 
                 input = head inputs
                 rest = tail inputs
         Out pos -> (updatePC 2 state, [getValue state (head modes) pos])
         JmpT bool pos -> (setPC pc' state, [])
             where
                 pc' = if getValue state (head modes) bool /= 0
                       then getValue state (head $ tail modes) pos
                       else pc + 3
         JmpF bool pos -> (setPC pc' state, [])
             where
                 pc' = if getValue state (head modes) bool == 0
                       then getValue state (head $ tail modes) pos
                       else pc + 3
         LTi a b pos -> (updatePC 4 $ setValue pos newval (modes !! 2) state, [])
             where
                 a' = getValue state (head modes) a
                 b' = getValue state (head $ tail modes) b
                 newval = if a' < b' then 1 else 0
         EQi a b pos -> (updatePC 4 $ setValue pos newval (modes !! 2) state, [])
             where
                 a' = getValue state (head modes) a
                 b' = getValue state (head $ tail modes) b
                 newval = if a' == b' then 1 else 0
         RelA adjust -> (updatePC 2 $ state & rel +~ val, [])
             where val = getValue state (head modes) adjust
         Halt -> (ProgState inputs S.Empty 0 0, [])

runUntilOutput :: ProgState -> Writer [Integer] ProgState
runUntilOutput state = fromMaybe undefined $ find 
    (\x -> let (st, w) = runWriter x in
               w /= [] || st ^. program == S.Empty)
    (iterate (>>= intcodeStep) (writer (state, [])))

runIntcode :: Seq Integer -> [Integer] -> Writer [Integer] ProgState
runIntcode xs inputs = fromMaybe (writer (ProgState inputs S.Empty 0 0, [])) $ find 
    (\x -> (_program . fst) (runWriter x) == S.Empty) 
    (iterate (>>= intcodeStep) (writer (ProgState inputs xs 0 0, [])))

