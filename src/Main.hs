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
  , problemRules :: RulesTrie
  }
  deriving (Show)

data Rule = Rule {
    ruleFrom :: [Bool]
  , ruleTo :: Bool
  }
  deriving (Show)

type Solver a = St.StateT IS.IntSet IO a

data RulesTrie =
    EmptyTrie
  | Leaf Bool
  | Branch {
      branchFalse :: RulesTrie,
      branchTrue :: RulesTrie
    }
  deriving (Show)

insertTrie :: [Bool] -> Bool -> RulesTrie -> RulesTrie
insertTrie [] r EmptyTrie = Leaf r
insertTrie (x : xs) r EmptyTrie
  | x = Branch EmptyTrie (insertTrie xs r EmptyTrie)
  | otherwise = Branch (insertTrie xs r EmptyTrie) EmptyTrie
insertTrie (x : xs) r (Branch f t)
  | x = Branch f (insertTrie xs r t)
  | otherwise = Branch (insertTrie xs r f) t
insertTrie path r trie =
  error $ "unexpected: " ++ show path ++ ", " ++ show r ++ ", " ++ show trie

mkTrie :: [Rule] -> RulesTrie
mkTrie rules = foldr insertRule EmptyTrie rules
  where
    insertRule r trie =
      insertTrie (ruleFrom r) (ruleTo r) trie

    ruleList set = [i `IS.member` set | i <- [-2 .. 2]]

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
  from <- replicateM 5 pPot
  string " => "
  to <- pPot
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
  return $ Problem initSt $ mkTrie rules

checkRules :: Int -> RulesTrie -> Solver (Maybe Bool)
checkRules i trie = do
    check (-2) trie
  where
    check :: Int -> RulesTrie -> Solver (Maybe Bool)
    check 3 (Leaf r) = return $ Just r
    check _ EmptyTrie = return Nothing
    check j rules = do
      oldGeneration <- get
      let actualPot = (i+j) `IS.member` oldGeneration
      let rules' = if actualPot
                     then branchTrue rules
                     else branchFalse rules
      check (j+1) rules'

runGeneration :: Integer -> Problem -> Solver Bool
runGeneration generationNumber (Problem {..}) = do
    oldGeneration <- get
    when (generationNumber `mod` 10000 == 0) $
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

solve :: Integer -> Problem -> Solver ()
solve generations p@(Problem {..}) = go 1
  where
    go n = do
      if n == generations+1
        then return ()
        else do
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
  [generations, path] <- getArgs
  r <- parseFromFile pProblem path
  case r of
    Left err -> fail (show err)
    Right problem -> do
      generation <- execStateT (solve (read generations) problem) (problemInitState problem)
      putStrLn $ showGeneration generation
      let answer = sum $ IS.toList generation
      print answer

