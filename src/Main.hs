{-
symon - minimal version of the classic electronic memory game
using only standard libs; command line, no sound.

This was an attempt to submit a ludum dare entry in 2h.

Time log (. = ~15m):
research game history, naming situation  .
project setup, git hassle  .
randomness, random adt hassle  ..
play a single game with random integers, hassle setting up a while loop  ...
package cleanup, docs, publishing  .
discussion  .
show one number at a time  .
read numbers with timeout  .
rendering improvements  ..

Issues:
Why does rendering work as intended in GHCI but not when compiled ?
Avoid showing multiple digits after rapid keypresses - how to disable terminal echo ?

-}

-- {-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Data.Maybe
import System.Console.ANSI
import System.Exit
import System.IO
-- import System.Posix.Terminal
import System.Random
import System.Timeout

type Tone = Int  -- 1 to 4

main :: IO ()
main = do
  game

game = do
  hSetBuffering stdout NoBuffering
  hideCursor
  g <- newStdGen
  let seq = take 10 $ randomRs (1,4::Tone) g
  userseq <- flip takeWhileM [1..length seq] $ \n -> do
    let seqsofar = take n seq
    setCursorColumn 0
    clearLine
    threadDelay 1000000
    playTones seqsofar
    ss <- getTones n
    return $ ss == map show seqsofar
  let score = length userseq
  putStrLn $ "Your score: " ++ show score
  when (score == length seq) $
    putStrLn "You won! Congratulations."
  showCursor

playTones ts = do
  forM_ ts $ \tone -> do
    setCursorColumn 0
    putStr $ show tone
    threadDelay 1000000
    clearLine
    setCursorColumn 0
    threadDelay 500000

getTones :: Int -> IO [String]
getTones n = do
  cs <- forM [1..n] $ \_ -> do
    c <- timeout 5000000 getChar
    setCursorColumn 0
    clearLine
    putStr $ (fromMaybe ' ' c):""
    threadDelay 500000
    setCursorColumn 0
    clearLine
    return c
  return $ map fromJust $ takeWhile isJust $ map ((:"") <$>) cs


