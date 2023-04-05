{-
-- EPITECH PROJECT, 2022
-- imageCompressor
-- File description:
-- Lib.hs
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( -- charToInt,
    distance,
    -- ok,
    -- closest,
    startCluster,
    Point (..),
    Cluster (..),
    kMeans,
    strToPoint,
  )
where

import Control.Monad (replicateM)
-- import Data.Char (isDigit, ord)
import Data.Function (on)
-- import Data.List (transpose)
import System.Random (randomRIO)

data Point = Point
  { r :: Int,
    g :: Int,
    b :: Int,
    posx :: Int,
    posy :: Int
  }
  deriving (Show)

-- instance Num Point where
--   (+) (Point r1 g1 b1) (Point r2 g2 b2) = Point (r1 + r2) (g1 + g2) (b1 + b2)
--   (*) (Point r1 g1 b1) (Point r2 g2 b2) = Point (r1 * r2) (g1 * g2) (b1 * b2)
--   (-) (Point r1 g1 b1) (Point r2 g2 b2) = Point (r1 - r2) (g1 - g2) (b1 - b2)
--   abs (Point r1 g1 b1) = Point (abs r1) (abs g1) (abs b1)
--   signum (Point r1 g1 b1) = Point (signum r1) (signum g1) (signum b1)
--   fromInteger i = Point (fromInteger i) (fromInteger i) (fromInteger i)

-- instance Fractional Point where
--   (/) (Point r1 g1 b1) (Point r2 g2 b2) = Point (r1 `div` r2) (g1 `div` g2) (b1 `div` b2)
--   fromRational r = Point (fromIntegral $ round r) (fromIntegral $ round r) (fromIntegral $ round r)

instance Eq Point where
  (==) = (==) `on` (\(Point r g b posx posy) -> (r, g, b, posx, posy))

-- instance Ord Point where
--   compare = compare `on` (\(Point r g b) -> (r, g, b))

data Cluster = Cluster
  { centroid :: Point,
    points :: [Point]
  }
  deriving (Show)

instance Eq Cluster where
  c1 == c2 = centroid c1 == centroid c2 && points c1 == points c2

-- charToInt :: Char -> Float
-- charToInt c = fromIntegral (ord c - ord '0')
distance :: Point -> Point -> Float
distance point1 point2 =
  sqrt
    ( fromIntegral
        ( (r point1 - r point2) ^ 2
            + (g point1 - g point2) ^ 2
            + (b point1 - b point2) ^ 2
        )
    )

-- ok :: String -> [Float]
-- ok [] = []
-- ok (x : xs)
--   | x <= '9' && x >= '0' = charToInt x : ok xs
--   | otherwise = ok xs

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find replaceWith (x : xs)
  | x == head find = head replaceWith : replace find replaceWith xs
  | otherwise = x : replace find replaceWith xs

parseTuple :: String -> [Int]
parseTuple s = map read $ filter (not . null) $ splitByComma (replace ")" " " $ replace "(" " " s)
  where
    splitByComma = foldr (\c acc -> if c == ',' then [] : acc else (c : head acc) : tail acc) [[]]

-- closest :: [Point] -> Point -> Point -> Int
-- closest [] _ z = z
-- closest (x : xs) y z
--   | distance (ok x) y < distance (ok z) y = closest xs y x
--   | otherwise = closest xs y z

strToCluster :: String -> Cluster
strToCluster str = Cluster {centroid = Point (head (parseTuple pint)) (parseTuple pint !! 1) (parseTuple pint !! 2) (head (parseTuple f)) (last (parseTuple f)), points = []}
  where
    pint = last (words str)
    f = head (words str)

checkIndices :: [Int] -> Bool
checkIndices [] = True
checkIndices (x : xs)
  | x `elem` xs = False
  | otherwise = checkIndices xs

getRdmIdc :: [String] -> Int -> IO [Int]
getRdmIdc x k = do
  indices <- replicateM k $ randomRIO (0, length x - 1)
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
strToPoint (x : xs) = Point (head (parseTuple pint)) (parseTuple pint !! 1) (parseTuple pint !! 2) (head (parseTuple f)) (last (parseTuple f)) : strToPoint xs
  where
    pint = last (words x)
    f = head (words x)

getMin :: Point -> [Cluster] -> Float -> Int -> Int -> Int
getMin _ [] _ _ res = res
getMin x (y : ys) mins i res
  | distance x (centroid y) < mins = getMin x ys (distance x (centroid y)) (i + 1) i
  | otherwise = getMin x ys mins (i + 1) res

-- meanPoint :: [Point] -> Point
-- meanPoint ps = sum ps / fromIntegral (length ps)

-- updCluster :: [Cluster] -> [Point] -> [Cluster]
-- updCluster [] _ = []
-- updCluster (x : xs) cluster
--   | not (null (points x)) = Cluster {centroid = meanPoint (points x), points = []} : updCluster xs cluster
--   | otherwise = x : updCluster xs cluster

-- updPoint :: [Cluster] -> [Point] -> [Cluster]
-- updPoint cluster [] = cluster
-- updPoint cluster (x : xs) = do
--   let newCluster = cluster !! (getMin (centroid x) cluster (distance x (head cluster)) 0) {points = x : points}
--   updPoint cluster xs
-- updPoint :: [Cluster] -> [Point] -> [Cluster]
-- updPoint = foldr updatePoint
--   where
--     updatePoint :: Point -> [Cluster] -> [Cluster]
--     updatePoint point [] = [Cluster point [point]]
--     updatePoint point (cluster : rest) =
--       let closestCluster = foldr (\c1 c2 -> if distance (centroid c1) point < distance (centroid c2) point then c1 else c2) cluster rest
--        in closestCluster {points = point : points closestCluster} : filter (/= closestCluster) rest

convergence :: [Cluster] -> [Cluster] -> Float -> Bool
convergence [] [] _ = True
convergence (c1 : cs1) (c2 : cs2) threshold
  | distance (centroid c1) (centroid c2) <= threshold = convergence cs1 cs2 threshold
  | otherwise = False

-- kMeans :: [Cluster] -> [Point] -> Float -> [Cluster]
-- kMeans cluster points c = do
--   let newCluster = updCluster cluster points
--   let newPoint = updPoint newCluster points
--   if convergence newCluster cluster c
--     then newPoint
--     else kMeans newPoint points c

getNewPos :: Cluster -> [Point] -> Int -> Int -> Int -> Int -> Point
getNewPos c [] sumR sumG sumB count
  | count == 0 = (centroid c) {r = sumR, g = sumG, b = sumB}
  | otherwise = (centroid c) {r = sumR `div` count, g = sumG `div` count, b = sumB `div` count}
getNewPos c (x : xs) sumR sumG sumB count = getNewPos c xs (sumR + r x) (sumG + g x) (sumB + b x) (count + 1)

calcNew :: [Cluster] -> [Cluster]
calcNew [] = []
calcNew (x : xs) = x {centroid = getNewPos x (points x) 0 0 0 0, points = []} : calcNew xs

-- orgNew :: [Point] -> [Cluster] -> [Cluster]
-- orgNew [] cluster = cluster
-- orgNew (x : xs) cluster = orgNew xs (cluster !! (getMin x cluster 1000 0 0) {points = x : points})

orgNew :: [Point] -> [Cluster] -> [Cluster]
orgNew [] clusters = clusters
orgNew (x : xs) clusters =
  let closestCluster = foldr (\c1 c2 -> if distance (centroid c1) x < distance (centroid c2) x then c1 else c2) (head clusters) clusters
      updatedCluster = closestCluster {points = x : points closestCluster}
      restClusters = filter (/= closestCluster) clusters
   in orgNew xs (updatedCluster : restClusters)

kMeans :: [Point] -> [Cluster] -> Float -> [Cluster]
kMeans allp clusters threshold
  | convergence (calcNew clusters) clusters threshold = orgNew allp (calcNew clusters)
  | otherwise = kMeans allp (orgNew allp (calcNew clusters)) threshold
