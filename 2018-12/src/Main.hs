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
    problemInitState :: Generation
  , problemRules :: RulesTrie
  }
  deriving (Show)

data Rule = Rule {
    ruleFrom :: [Bool]
  , ruleTo :: Bool
  }
  deriving (Show)

type Solver a = St.StateT Generation IO a

data RulesTrie =
    EmptyTrie
  | Leaf Bool
  | Branch {
      branchFalse :: RulesTrie,
      branchTrue :: RulesTrie
    }
  deriving (Show)

type Generation = IS.IntSet

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

mkSet :: [Int] -> [Bool] -> Generation
mkSet idxs mask =
  IS.fromList [i | (i, m) <- zip idxs mask, m]

pInitState :: Parser Generation
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

compareGenerations :: Generation -> Generation -> Maybe Int
compareGenerations old new = check (-2)
  where
    check d
      | d == 3 = Nothing
      | IS.map (+d) old == new = Just d
      | otherwise = check (d+1)

-- Nothing - generations are totally different
-- Just (n, d) - at generation n, new generation is shifted w.r.t old one with shift == d
runGeneration :: Integer -> Problem -> Solver (Maybe (Integer, Int))
runGeneration generationNumber (Problem {..}) = do
    oldGeneration <- get
    when (generationNumber `mod` 9999 == 0) $
        lift $ putStrLn $ show generationNumber ++ ": " ++ showGeneration oldGeneration

    newPairs <-
        forM [IS.findMin oldGeneration - 3 .. IS.findMax oldGeneration + 3] $ \i -> do
          r <- checkRules i problemRules
          return (i, fromMaybe False r)
    
    let newGeneration = uncurry mkSet (unzip newPairs)
    case compareGenerations oldGeneration newGeneration of
      Just d -> return $ Just (generationNumber, d)
      Nothing -> do
           put newGeneration
           return Nothing

-- Nothing - we have counted to the end
-- Just (n, d) - we stopped at n'th generation and shift == d
solve :: Integer -> Problem -> Solver (Maybe (Integer, Int))
solve generations p@(Problem {..}) = go 1
  where
    go n = do
      if n == generations+1
        then return Nothing
        else do
          r <- runGeneration n p
          case r of
            Nothing -> go (n+1)
            Just (stop, d) -> return $ Just (stop, d)

showGeneration :: Generation -> String
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
  [generationsS, path] <- getArgs
  let generations = read generationsS
  r <- parseFromFile pProblem path
  case r of
    Left err -> fail (show err)
    Right problem -> do
      (result, generation) <- runStateT (solve generations problem) (problemInitState problem)
      putStrLn $ showGeneration generation
      let currentSum = sum $ IS.toList generation
      print result
      case result of
        Nothing -> do
          print currentSum
        Just (stop, shift) -> do
          let generationsLeft = generations - stop + 1
              shiftLeft = fromIntegral shift * generationsLeft
              nPlants = fromIntegral $ IS.size generation
              answer = fromIntegral currentSum + nPlants * shiftLeft
          print answer

