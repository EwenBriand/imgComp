{-
-- EPITECH PROJECT, 2022
-- compressor
-- File description:
-- Main.hs
-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Lib
    ( kMeans,
      startCluster,
      strToPoint,
      Cluster(points, centroid),
      Point(b, posx, posy, r, g) )
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- import Text.Printf (PrintfArg(parseFormat))

data Options = Options
  { file :: String,
    cluster :: [Point], -- centroid
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
  parseOptions xs >>= (\o -> Just o
  {nbrColors = Just (fromMaybe 1 (readMaybe n :: Maybe Int))})

parseOptions ("-l" : c : xs) =
  parseOptions xs >>= (\o -> Just o
  {convergence = Just (fromMaybe 1 (readMaybe c :: Maybe Float))})

parseOptions ("-f" : f : xs) =
  parseOptions xs >>= (\o -> Just o {file = f})

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

  -- putStrLn (show (nbrColors opt))
  -- putStrLn (show (convergence opt))
  -- putStrLn content

  let li = lines content
  clusters <- startCluster li (maybeToInt (nbrColors opt))
  let allp = strToPoint li
  -- print clusters

  let res = kMeans 1 allp clusters (maybeToFloat (convergence opt))
  -- let res = kMeans allp clusters 10
  -- let centroids = printCentroids res
  -- mapM_ print centroids
  printOutputs res

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let options = parseOptions args
--   -- print options
--   case options of
--     Just o -> do
--       if convergence o == Nothing
--         then print "Error: convergence is not set"
--         else
--           if nbrColors o == Nothing
--             then print "Error: nbrColors is not set"
--             else
--               if file o == ""
--                 then print "Error: file is not set"
--                 else parseFile options
--     Nothing -> print "Error: Failed to parse options"

main :: IO ()
main = do
  args <- getArgs
  let options = parseOptions args
  -- print options
  case options of
    Just o | convergence o == Nothing -> print "Error: convergence is not set"
           | nbrColors o == Nothing -> print "Error: nbrColors is not set"
           | file o == "" -> print "Error: file is not set"
           | otherwise -> parseFile options
    Nothing -> print "Error: Failed to parse options"
