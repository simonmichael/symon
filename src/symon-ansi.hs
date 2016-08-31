{-
symon-ansi - minimal version of the classic electronic memory game
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
rendering improvement, terminal mode issues  ....
readme, description  ...
rename  .
restore terminal  .
release, cleanup  .

-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Maybe
import System.Console.ANSI
import System.Exit
import System.IO
import System.Random
import System.Timeout

type Tone = Int  -- 1 to 4

main :: IO ()
main = bracket setupTerminal (const restoreTerminal) (const runGame)

setupTerminal = do
  hideCursor
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

restoreTerminal = do
  showCursor
  hSetEcho stdin True

runGame = do
  g <- newStdGen
  let seq = take 10 $ randomRs (1,4::Tone) g
  userseq <- flip takeWhileM [1..length seq] $ \n -> do
    let seqsofar = take n seq
    setCursorColumn 0
    clearLine
    threadDelay 500000
    playTones seqsofar
    ss <- getTones n
    return $ ss == map show seqsofar
  showScore seq userseq

playTones ts = do
  forM_ ts $ \tone -> do
    setCursorColumn 0
    putStr $ show tone
    threadDelay 500000
    clearLine
    setCursorColumn 0
    threadDelay 400000

getTones :: Int -> IO [String]
getTones n = do
  cs <- forM [1..n] $ \_ -> do
    c <- timeout 5000000 getChar
    when (c==Just 'q') $ restoreTerminal >> exitSuccess
    setCursorColumn 0
    clearLine
    putStr $ (fromMaybe ' ' c):""
    threadDelay 500000
    setCursorColumn 0
    clearLine
    return c
  return $ map fromJust $ takeWhile isJust $ map ((:"") <$>) cs

showScore seq userseq = do
  let score = length userseq
  putStrLn $ "Your score: " ++ show score
  if (score == length seq)
  then
    putStrLn "You won! Congratulations."
  else
    putStrLn "Better luck next time."

