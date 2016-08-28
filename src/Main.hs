{-
symon - minimal version of the classic electronic memory game
using only standard libs; command line, no sound.

Attempting to submit a ludum dare entry in 2h. Time log (. = ~15m):
research  .
 ; check game status, history, naming situation
project setup, git hassle  .
 ; git commit --amend hangs today for the first time. Fire up intellij to commit
randomness  ..
 ; http://stackoverflow.com/questions/11811498/generate-a-random-value-from-a-user-defined-data-type-in-haskell
 ; why so complicated ?
play a single game with random integers; setting up a while loop  ...
package cleanup, docs, publishing  .

-}

-- {-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad
import Control.Monad.Loops
import System.Exit
import System.IO
import System.Random

-- data Tone = L | R | U | D deriving (Show, Eq)

type Tone = Int  -- 1 to 4

main :: IO ()
main = do
  game

game = do
  g <- newStdGen
  let seq = take 10 $ randomRs (1::Tone,4) g
  successes <- flip takeWhileM [1..length seq] $ \step -> do
    playTones seq step
    i <- getLine
    return $ words i == map show (take step seq)
  let score = length successes
  putStrLn $ "Your score: " ++ show score
  when (score == length seq) $
    putStrLn "You won! Congratulations."

playTones seq step = do
  putStrLn $ unwords $ map show $ take step seq
