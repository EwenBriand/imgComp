{-
-- EPITECH PROJECT, 2022
-- imageCompressor
-- File description:
-- Lib.hs
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  (
    distance,
    startCluster,
    Point (..),
    Cluster (..),
    kMeans,
    strToPoint,
  )
where

import Control.Monad (replicateM)
import Data.Function (on)
import System.Random

data Point = Point
  { r :: Int,
    g :: Int,
    b :: Int,
    posx :: Int,
    posy :: Int
  }
  deriving (Show)

instance Eq Point where
  (==) = (==) `on` (\(Point r g b posx posy) -> (r, g, b, posx, posy))

data Cluster = Cluster
  { centroid :: Point,
    points :: [Point]
  }
  deriving (Show)

instance Eq Cluster where
  c1 == c2 = centroid c1 == centroid c2 && points c1 == points c2

distance :: Point -> Point -> Float
distance point1 point2 =
  sqrt
    ( fromIntegral
        ( (r point1 - r point2) ^ 2
            + (g point1 - g point2) ^ 2
            + (b point1 - b point2) ^ 2
        )
    )

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find replaceWith (x : xs)
  | x == head find = head replaceWith : replace find replaceWith xs
  | otherwise = x : replace find replaceWith xs

parseTuple :: String -> [Int]
parseTuple s = map read $ filter (not . null)
  $ splitByComma (replace ")" " " $ replace "(" " " s)
  where
    splitByComma = foldr (\c acc -> if c == ','
      then [] : acc else (c : head acc) : tail acc) [[]]

strToCluster :: String -> Cluster
strToCluster str = Cluster {centroid = poin, points = []}
  where
    pint = last (words str)
    f = head (words str)
    poin = Point (head (parseTuple pint)) (parseTuple pint !! 1)
      (parseTuple pint !! 2) (head (parseTuple f)) (last (parseTuple f))

checkIndices :: [Int] -> Bool
checkIndices [] = True
checkIndices (x : xs)
  | x `elem` xs = False
  | otherwise = checkIndices xs

getRdmIdc :: [String] -> Int -> IO [Int]
getRdmIdc x k = do
  gen <- newStdGen
  let indices = take k $ randomRs (0, length x - 1) gen
  if checkIndices indices
    then return indices
    else getRdmIdc x k

createCluster :: [String] -> Int -> IO [Cluster]
createCluster _ 0 = return []
createCluster x k = do
  indices <- getRdmIdc x k
  return $ map (strToCluster . (x !!)) indices

startCluster :: [String] -> Int -> IO [Cluster]
startCluster = createCluster

strToPoint :: [String] -> [Point]
strToPoint [] = []
strToPoint (x : xs) = Point (head (parseTuple pint))
  (parseTuple pint !! 1) (parseTuple pint !! 2)
  (head (parseTuple f)) (last (parseTuple f)) : strToPoint xs
  where
    pint = last (words x)
    f = head (words x)

convergence :: [Cluster] -> [Cluster] -> Float -> Bool
convergence [] [] _ = True
convergence (c1 : cs1) (c2 : cs2) threshold
  | distance (centroid c1) (centroid c2) <=
    threshold = convergence cs1 cs2 threshold
  | otherwise = False

getNewPos :: Cluster -> [Point] -> Int -> Int -> Int -> Int -> Point
getNewPos c xs sumR sumG sumB count
  = case xs of
      [] -> if count == 0
              then (centroid c) {r = sumR, g = sumG, b = sumB}
              else (centroid c) {r = sumR `div` count,
                                  g = sumG `div` count,
                                  b = sumB `div` count}
      x : xs' -> getNewPos c xs' (sumR + r x) (sumG + g x)
                (sumB + b x) (count + 1)


calcNew :: [Cluster] -> [Cluster]
calcNew =
  map
    (\x -> x {centroid = getNewPos x (points x) 0 0 0 0, points = []})

closest :: Point -> [Cluster] -> Cluster -> Cluster
closest _ [] best = best
closest p (x : xs) best
  | distance (centroid x) p < distance (centroid best) p = closest p xs x
  | otherwise = closest p xs best

orgNew :: [Point] -> [Cluster] -> [Cluster]
orgNew [] clusters = clusters
orgNew (x : xs) clusters =
  let closestCluster = closest x clusters (head clusters)
      updatedCluster = closestCluster {points = x : points closestCluster}
      restClusters = filter (/= closestCluster) clusters
    in orgNew xs (updatedCluster : restClusters)

kMeans :: Int -> [Point] -> [Cluster] -> Float -> [Cluster]
kMeans 1 allp clusters threshold =
  kMeans 0 allp (orgNew allp clusters) threshold
kMeans _ allp clusters threshold
  | convergence (calcNew clusters) clusters threshold =
    orgNew allp (calcNew clusters)
  | otherwise = kMeans 0 allp (orgNew allp (calcNew clusters)) threshold
