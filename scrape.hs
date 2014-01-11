import Control.Arrow 
import Control.Monad

import System.Exit (exitFailure)

import Network.Curl.Download (openURIString)
import Network.Curl.Code

import Data.List (isInfixOf)
import Data.List.Split (splitOn)

composeUri :: String -> String
composeUri = (++) "http://www.urbanspoon.com/"

processLine :: String -> String
processLine = 
  splitOn "href=\"" >>> tail >>> head >>>
  splitOn "\"" >>> head

processPage :: String -> IO Bool
processPage url = do
  let url' = composeUri url
  putStrLn $ "Processing: '" ++ url' ++ "'"
  fetched <- openURIString url'
  let pageText = either (\e -> error "Failed: " ++ show e) id fetched
  let yesno = "rez." `isInfixOf` pageText
  print yesno
  return yesno

main :: IO ()
main = do
  fetched <- openURIString "http://www.urbanspoon.com/n/6/563/SF-Bay/Western-Addition-restaurants"
  case fetched of
    Left e -> do
      putStrLn "Failed fetching page!"
      putStrLn e
      exitFailure
    Right s -> do
      let filterRestaurants = filter (isInfixOf "/restaurant/") >>> map processLine
          restaurants = filterRestaurants $ lines s
      print $ length restaurants
      rezs <- filterM processPage restaurants
      print rezs
      return ()
