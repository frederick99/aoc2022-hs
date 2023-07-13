{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day8 where

import Data.Maybe ( fromJust, isJust )
import qualified Data.Set as Set
import qualified Data.Map as Map

data Opcode  = NoOp | Incr | Jump
type Arg     = Int
data Instr   = Instr Opcode Arg
data Context = Context { program :: Map.Map Int Instr
                       , accumulator :: Int
                       , current_instr :: Int
                       , instr_history :: Set.Set Int
                       }

mkOp "nop" = NoOp
mkOp "acc" = Incr
mkOp "jmp" = Jump

mkArg ('+' : arg) = read arg
mkArg ('-' : arg) = negate $ read arg

mkInstr (words -> [mkOp -> op, mkArg -> arg]) = Instr op arg

mkContext instrs = Context (Map.fromList $ zip [0..] instrs) 0 0 mempty

runInstr :: Context -> Maybe Context
runInstr (Context code acc pc history) = run <$> Map.lookup pc code
  where
    run (Instr NoOp _) = Context code  acc      (pc + 1) history'
    run (Instr Incr x) = Context code (acc + x) (pc + 1) history'
    run (Instr Jump k) = Context code  acc      (pc + k) history'
    history' = Set.insert pc history

execute = map fromJust . takeWhile isJust
        . iterate (>>= runInstr)
        . return . mkContext

isHalting = null . (drop . length <*> tail . execute)

instrs = map mkInstr . lines <$> readFile "src/input/Day8.txt"

------------------------------------------------------
partOne = print . accumulator . snd
        . last  . takeWhile didn'tLoop
        . zip [0..] . execute =<< instrs
  where didn'tLoop (i, length . instr_history -> n) = i == n
-- Answer: 2080

mutations (instr : rest)
  | Instr NoOp arg <- instr = (Instr Jump arg : rest) : restMutations
  | Instr Incr arg <- instr =                           restMutations
  | Instr Jump arg <- instr = (Instr NoOp arg : rest) : restMutations
  where restMutations = (instr :) <$> mutations rest
mutations [] = []

partTwo = print . accumulator
        . last . execute
        . head . filter isHalting
        . mutations =<< instrs
-- Answer: 2477
