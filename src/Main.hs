{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.State as St
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Data.IntSet as IS
import System.Environment

data Problem = Problem {
    problemInitState :: IS.IntSet
  , problemRules :: [Rule]
  }
  deriving (Show)

data Rule = Rule {
    ruleFrom :: IS.IntSet
  , ruleTo :: Bool
  }
  deriving (Show)

type Solver a = St.StateT IS.IntSet IO a

mkSet :: [Int] -> [Bool] -> IS.IntSet
mkSet idxs mask =
  IS.fromList [i | (i, m) <- zip idxs mask, m]

pInitState :: Parser IS.IntSet
pInitState = do
  string "initial state: "
  st <- manyTill pPot newline 
  return $ mkSet [0..] st

pRule :: Parser Rule
pRule = do
  fromSt <- replicateM 5 pPot
  string " => "
  to <- pPot
  let from = mkSet [-2 .. 2] fromSt
  return $ Rule from to

pPot :: Parser Bool
pPot = do
  pot <- oneOf ['#', '.']
  return $ pot == '#'

pProblem :: Parser Problem
pProblem = do
  initSt <- pInitState
  newline
  rules <- pRule `sepEndBy` newline
  return $ Problem initSt rules

checkRule :: Int -> Rule -> Solver (Maybe Bool)
checkRule i (Rule {..}) = do
    matches <- check (-2)
    if matches
      then return $ Just ruleTo
      else return Nothing
  where
    check :: Int -> Solver Bool
    check 3 = return True
    check j = do
      oldGeneration <- get
      let rulePot = j `IS.member` ruleFrom
          actualPot = (i+j) `IS.member` oldGeneration
      if rulePot == actualPot
        then check (j+1)
        else return False

checkRules :: Int -> [Rule] -> Solver (Maybe Bool)
checkRules _ [] = return Nothing
checkRules i (rule : rules) = do
  r <- checkRule i rule
  case r of
    Nothing -> checkRules i rules
    Just new -> return (Just new)

runGeneration :: Integer -> Problem -> Solver Bool
runGeneration generationNumber (Problem {..}) = do
    oldGeneration <- get
    when (generationNumber `mod` 1000 == 0) $
        lift $ putStrLn $ show generationNumber ++ ": " ++ showGeneration oldGeneration

    newPairs <-
        forM [IS.findMin oldGeneration - 3 .. IS.findMax oldGeneration + 3] $ \i -> do
          r <- checkRules i problemRules
          return (i, fromMaybe False r)
    
    let newGeneration = uncurry mkSet (unzip newPairs)
    if newGeneration == oldGeneration
      then return True
      else do
           put newGeneration
           return False

solve :: Problem -> Solver ()
solve p@(Problem {..}) = go 1
  where
    go 50000000000 = return ()
    go n = do
      stop <- runGeneration n p
      if stop
        then return ()
        else go (n+1)

showGeneration :: IS.IntSet -> String
showGeneration gen = 
    if IS.null gen
      then "<EMPTY>"
      else show iMin ++ " |" ++ [showPot (i `IS.member` gen) | i <- [iMin .. iMax]]
  where
    iMin = IS.findMin gen
    iMax = IS.findMax gen
    showPot True = '#'
    showPot False = '.'

main :: IO ()
main = do
  [path] <- getArgs
  r <- parseFromFile pProblem path
  case r of
    Left err -> fail (show err)
    Right problem -> do
      generation <- execStateT (solve problem) (problemInitState problem)
      putStrLn $ showGeneration generation
      let answer = sum $ IS.toList generation
      print answer

