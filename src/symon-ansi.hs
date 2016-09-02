{-
symon-cli - minimal version of the classic electronic memory game
Few dependencies, command line, no sound.

This was an attempt to submit a ludum dare entry in 2h.

Time log (. = ~15m):
2016/8
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
release, cleanup  ..

research  ..
discussion  .

2016/9
refactor  .
rendering  .

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

type Tone = Char -- '1' to '4'
tfirst = '1'
tlast = '4'

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
  let seq = take 10 $ randomRs (tfirst,tlast) g
  userseq <- flip takeWhileM [1..length seq] $ \n -> do
    let seqsofar = take n seq
    playSilence 400000
    playTones seqsofar
    ts <- getTones n
    return $ ts == seqsofar
  showScore seq userseq

playTones ts =
  forM_ ts $ \t -> do
    playTone t 500000
    playSilence 400000

getTones :: Int -> IO [Char]
getTones n = do
  cs <- forM [1..n] $ \_ -> do
    c <- timeout 5000000 getChar
    when (c==Just 'q') $ restoreTerminal >> exitSuccess
    playTone (fromMaybe ' ' c) 500000
    playSilence 400000
    return c
  return $ map fromJust $ takeWhile isJust cs

playSilence interval = do
  setCursorColumn 0
  forM_ [tfirst..tlast] $ \t -> putChar t
  threadDelay interval

playTone tone interval = do
  setCursorColumn 0
  forM_ [tfirst..tlast] $ \t ->
    if t == tone
    then do
--       setSGR [SetColor Foreground Vivid Red]
      setSGR [SetColor Foreground Vivid Red]
      putChar t
      setSGR [SetColor Foreground Dull Black]
    else do
      putChar t
  threadDelay interval

showScore seq userseq = do
  let score = length userseq
  putStrLn $ "Your score: " ++ show score
  if (score == length seq)
  then
    putStrLn "You won! Congratulations."
  else
    putStrLn "Better luck next time."

