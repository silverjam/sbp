#!/usr/bin/runhaskell

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

import System.Cmd (system)
import System.Exit

import Data.Time.LocalTime

import Control.Concurrent (threadDelay)

toMSecs :: Num a => a -> a
toMSecs = (*) 1000000

sleepUntilMinuteBounary :: IO ()
sleepUntilMinuteBounary = do
  zonedTime <- getZonedTime
  let localTime = zonedTimeToLocalTime zonedTime
      localTod = localTimeOfDay localTime
  let x = 60 - floor (todSec localTod)
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

iff :: forall (m :: * -> *) a. Monad m => Bool -> m a -> m ()
iff True x = do { x; return () }
iff False _ = return ()

loop :: IO ()
loop = do
  fourAm <- isCloseToFourAm
  iff (not fourAm) sleepUntilMinuteBounary
  zonedTime <- getZonedTime
  let theTime = show zonedTime
  let delay = if fourAm then 2 else 60
  putStrLn $ ">>> Waiting " ++ show delay ++ " seconds to run the script (" ++ theTime ++ ")"
  threadDelay $ toMSecs delay
  putStrLn ">>> Running..."
  exitCode <- system "ruby ./urbanspoon_reservations.rb"
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      putStrLn "... Failed"
      loop

main :: IO ()
main = do
  loop
  exitFailure
