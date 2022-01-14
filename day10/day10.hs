#!/usr/bin/env -S cabal run -v0
{- cabal:
build-depends: base, split 
ghc-options: -Wall
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

import qualified Data.Bits                     as Bits
import qualified Data.Char                     as Char
import qualified Data.List                     as List
import qualified Data.List.Split               as List

rotateL :: Int -> [a] -> [a]
rotateL n l = (\(a, b) -> b <> a) . List.splitAt (n `mod` length l) $ l

runRound :: [Int] -> [Int]
runRound = go 0 0 [0 .. 255]
  where
    go currentPos skipSize list = \case
        (l : ls) ->
            let (toReverse, after) = List.splitAt l list
                newList = rotateL skipSize . (after <>) . reverse $ toReverse
            in  go ((currentPos + l + skipSize) `mod` length list)
                   (skipSize + 1)
                   newList
                   ls
        _ -> rotateL (length list - currentPos) list

runRounds :: Int -> [Int] -> [Int]
runRounds n = runRound . concat . replicate n

hash :: String -> String
hash input =
    let lengths =
            (<> [17, 31, 73, 47, 23])
                . map fromEnum
                . filter (not . Char.isSpace)
                $ input
        sparseHash = runRounds 64 lengths
        denseHash  = List.foldl1' Bits.xor <$> List.chunksOf 16 sparseHash
    in  concatMap (leftPad 2 '0' . convertToBase 16) denseHash

leftPad :: Int -> a -> [a] -> [a]
leftPad n x xs = if len > n then xs else drop len $ replicate n x <> xs
    where len = length xs

convertToBase :: Integral a => Int -> a -> String
convertToBase b n
    | n < 0
    = '-' : convertToBase b (-n)
    | n < fromIntegral b
    = [(['0' .. '9'] ++ ['a' .. 'z']) !! fromIntegral n]
    | otherwise
    = let (d, m) = n `divMod` fromIntegral b
      in  convertToBase b d ++ convertToBase b m

main :: IO ()
main = do
    ((a :: Int) : b : _) <-
        runRound
        .   fmap read
        .   List.splitOn ","
        .   filter (not . Char.isSpace)
        <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> show (a * b)

    knotHash <- hash <$> readFile "input.txt"
    putStrLn $ "Part 2: " <> knotHash
