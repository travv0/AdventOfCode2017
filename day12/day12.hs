#!/usr/bin/env -S cabal run -v0
{- cabal:
build-depends: base, split, containers
ghc-options: -Wall
-}

import qualified Data.List.Split               as List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           GHC.Exts                       ( fromList )

type Program = Int
type ProgramGroup = Set Program
type Pipes = Map Program ProgramGroup

parse :: String -> Pipes
parse =
    fromList
        . map
              (\line ->
                  let [program, connections] = List.splitOn " <-> " line
                  in  ( read program
                      , fromList . map read . List.splitOn ", " $ connections
                      )
              )
        . lines

programsInGroup :: Pipes -> Program -> ProgramGroup
programsInGroup pipes = go Set.empty
  where
    go result program
        | Set.member program result
        = result
        | otherwise
        = foldMap (go $ Set.insert program result) $ pipes Map.! program

programGroups :: Pipes -> Set ProgramGroup
programGroups pipes =
    foldr (Set.insert . programsInGroup pipes) Set.empty $ Map.keys pipes

main :: IO ()
main = do
    pipes <- parse <$> readFile "input.txt"

    putStrLn
        $  "There are "
        <> show (length $ programsInGroup pipes 0)
        <> " programs in the group that contains program ID 0"

    putStrLn
        $  "There are "
        <> show (length $ programGroups pipes)
        <> " groups in total"
