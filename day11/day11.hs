{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

import           Control.Applicative            ( (<|>) )
import qualified Data.Char                     as Char
import           Data.Foldable                  ( foldl' )
import qualified Data.List                     as List
import qualified Data.List.Split               as List
import qualified Data.Maybe                    as Maybe

data Dir = NW | N | NE | SE | S | SW
    deriving (Show, Eq, Enum)

clockwise :: Dir -> Dir
clockwise = clockwiseN 1

clockwiseN :: (Eq a, Num a) => a -> Dir -> Dir
clockwiseN 0 dir = dir
clockwiseN n dir = clockwiseN (n - 1) $ case dir of
    SW -> NW
    d  -> succ d

counterClockwise :: Dir -> Dir
counterClockwise = counterClockwiseN 1

counterClockwiseN :: (Eq a, Num a) => a -> Dir -> Dir
counterClockwiseN 0 dir = dir
counterClockwiseN n dir = counterClockwiseN (n - 1) $ case dir of
    NW -> SW
    d  -> pred d

parseDir :: String -> Dir
parseDir = \case
    "nw" -> NW
    "n"  -> N
    "ne" -> NE
    "sw" -> SW
    "s"  -> S
    "se" -> SE
    s    -> error $ "bad parse: " <> s

parse :: String -> [Dir]
parse = map parseDir . List.splitOn "," . filter (not . Char.isSpace)

replace :: forall a . Eq a => a -> a -> [a] -> Maybe [a]
replace elem with l = update <$> List.elemIndex elem l
  where
    update :: Int -> [a]
    update i = let (beg, _ : end) = List.splitAt i l in beg <> (with : end)

delete :: forall a . Eq a => a -> [a] -> Maybe [a]
delete elem l = remove <$> List.elemIndex elem l
  where
    remove :: Int -> [a]
    remove i = let (beg, _ : end) = List.splitAt i l in beg <> end

reducePath :: [Dir] -> ([Dir], Int)
reducePath = foldl'
    (\(path, furthest) dir ->
        let
            newPath =
                Maybe.fromMaybe (path <> [dir])
                    $   delete (clockwiseN 3 dir) path
                    <|> replace (clockwiseN 2 dir) (clockwiseN 1 dir) path
                    <|> replace (counterClockwiseN 2 dir)
                                (counterClockwiseN 1 dir)
                                path
            newFurthest = max furthest $ length newPath
        in
            (newPath, newFurthest)
    )
    ([], 0)

main :: IO ()
main = do
    (path, furthestDistance) <- reducePath . parse <$> readFile "input.txt"
    putStrLn $ "The child process is " <> show (length path) <> " steps away"
    putStrLn
        $  "The furthest he ever got from his starting position is "
        <> show furthestDistance
        <> " steps"
