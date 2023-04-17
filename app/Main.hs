{-
-- EPITECH PROJECT, 2022
-- compressor
-- File description:
-- Main.hs
-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Lib
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Options = Options
  { file :: String,
    cluster :: [Point],
    convergence :: Maybe Float,
    nbrColors :: Maybe Int
  }
  deriving (Show)

parseOptions :: [String] -> Maybe Options
parseOptions [] =
  Just
    Options
      { file = "",
        cluster = [],
        convergence = Nothing,
        nbrColors = Nothing
      }
parseOptions ("-n" : n : xs) =
  parseOptions xs >>= do
    let n' = readMaybe n :: Maybe Int
    let n'' = fromMaybe 1 n'
    (\o -> Just o {nbrColors = Just n''})
parseOptions ("-l" : c : xs) =
  parseOptions xs >>= do
    let c' = readMaybe c :: Maybe Float
    let c'' = fromMaybe 1 c'
    (\o -> Just o {convergence = Just c''})
parseOptions ("-f" : f : xs) =
  parseOptions xs >>= do
    (\o -> Just o {file = f})
parseOptions _ = Nothing

maybeToInt :: Maybe Int -> Int
maybeToInt Nothing = 0
maybeToInt (Just x) = x

maybeToFloat :: Maybe Float -> Float
maybeToFloat Nothing = 0
maybeToFloat (Just x) = x

printCentroids :: [Cluster] -> [Point]
printCentroids = map centroid

printPoint :: Point -> IO ()
printPoint p =
  putStrLn $
    "("
      ++ show (posx p)
      ++ ","
      ++ show (posy p)
      ++ ") ("
      ++ show (r p)
      ++ ","
      ++ show (g p)
      ++ ","
      ++ show (b p)
      ++ ")"

printCentroide :: Point -> IO ()
printCentroide p =
  putStrLn $
    "("
      ++ show (r p)
      ++ ","
      ++ show (g p)
      ++ ","
      ++ show (b p)
      ++ ")"

printList :: [Point] -> IO ()
printList = foldr ((>>) . printPoint) (return ())

printOutputs :: [Cluster] -> IO ()
printOutputs =
  foldr
    ( \x ->
        (>>)
          ( putStrLn "--"
              >> printCentroide (centroid x)
              >> putStrLn "-"
              >> printList (points x)
          )
    )
    (return ())

parseFile :: Maybe Options -> IO ()
parseFile Nothing = print "Error: Failed to parse options"
parseFile (Just opt) = do
  content <- readFile (file opt)

  let li = lines content
  clusters <- startCluster li (maybeToInt (nbrColors opt))
  let allp = strToPoint li
  let res = kMeans 1 allp clusters (maybeToFloat (convergence opt))
  printOutputs res

main :: IO ()
main = do
  args <- getArgs
  let options = parseOptions args
  case options of
    Just o -> do
      if convergence o == Nothing
        then print "Error: convergence is not set"
        else
          if nbrColors o == Nothing
            then print "Error: nbrColors is not set"
            else
              if file o == ""
                then print "Error: file is not set"
                else parseFile options
    Nothing -> print "Error: Failed to parse options"
