{-
-- EPITECH PROJECT, 2022
-- compressor
-- File description:
-- Main.hs
-}

module Main (main) where
import Lib ()
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
-- import Text.Printf (PrintfArg(parseFormat))

data Point = Point
    { r :: Int
    , g :: Int
    , b :: Int
    } deriving (Show)

data Options = Options
    { file :: String
    , cluster :: [Point] -- centroid
    , convergence :: Maybe Float
    , nbrColors :: Maybe Int
    } deriving (Show)

parseOptions :: [String] -> Maybe Options
parseOptions [] = Just Options
    { file = ""
    , cluster = []
    , convergence = Nothing
    , nbrColors = Nothing
    }

parseOptions ("-n" : n : xs) = parseOptions xs >>= do
    let n' = readMaybe n :: Maybe Int
    let n'' = fromMaybe 1 n'
    (\o -> Just o { nbrColors = Just n'' })

parseOptions ("-l" : c : xs) = parseOptions xs >>=  do
    let c' = readMaybe c :: Maybe Float
    let c'' = fromMaybe 1 c'
    (\o -> Just o { convergence = Just c'' })

parseOptions ("-f" : f : xs) = parseOptions xs >>= do
    (\o -> Just o { file = f })

parseOptions _ = Nothing

parseFile :: Maybe Options -> IO ()
parseFile Nothing = print "Error: Failed to parse options"
parseFile (Just opt) = do
    content <- readFile (file opt)
    putStrLn content

main :: IO ()
main = do
    args <- getArgs
    let options = parseOptions args
    -- print options
    case options of
        Just o -> do
            if convergence o == Nothing then
                print "Error: convergence is not set"
            else if nbrColors o == Nothing then
                print "Error: nbrColors is not set"
            else if file o == "" then
                print "Error: file is not set"
            else
                parseFile options

        Nothing -> print "Error: Failed to parse options"

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         [file] -> do
--             content <- readFile file
--             putStrLn content
--             -- parseFile content
--         _ -> putStrLn "Usage: ./compressor <file>"