#!/usr/bin/runhaskell

{-# LANGUAGE RankNTypes #-}

import System.Cmd (system)
import System.Exit

import Data.Time.LocalTime

import Control.Concurrent (threadDelay)

toMSecs :: Int -> Int
toMSecs = (*) 1000000

sleepUntilMinuteBounary :: IO ()
sleepUntilMinuteBounary = do
  zonedTime <- getZonedTime
  let localTime = zonedTimeToLocalTime zonedTime
      localTod = localTimeOfDay localTime
  let x = 60 - todMin localTod
  putStrLn $ ">>> Sleeping " ++ show x ++ " seconds to align with minute boundary..."
  threadDelay $ toMSecs x
  putStrLn "... Done"

isCloseToFourAm :: IO Bool
isCloseToFourAm = do
  zonedTime <- getZonedTime
  let localTime = zonedTimeToLocalTime zonedTime
      localTod = localTimeOfDay localTime
      (hour, minutes) = (todHour localTod, todMin localTod)
      close = (hour == 3 && minutes >= 59) || (hour == 4 && minutes <= 01)
  return close

loop :: IO ()
loop = do
  zonedTime <- getZonedTime
  let theTime = show zonedTime
  putStrLn $ ">>> Running script (" ++ theTime ++ ")"
  fourAm <- isCloseToFourAm
  let delay = if fourAm then 2 else 60
  threadDelay $ toMSecs delay
  exitCode <- system "ruby ./urbanspoon_reservations.rb"
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      putStrLn "... Failed"
      loop

main :: IO ()
main = do
  sleepUntilMinuteBounary
  loop
  exitFailure
