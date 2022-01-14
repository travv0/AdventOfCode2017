import qualified Data.List.Split               as List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           GHC.Exts                       ( fromList )

newtype Program = Program Int deriving (Show, Eq, Ord)
newtype Pipes = Pipes (Map Program (Set Program)) deriving Show

parse :: String -> Pipes
parse =
    Pipes
        . fromList
        . map
              (\line ->
                  let [program, connections] = List.splitOn " <-> " line
                  in  ( Program (read program)
                      , fromList
                          . map (Program . read)
                          . List.splitOn ", "
                          $ connections
                      )
              )
        . lines

programsInGroup :: Pipes -> Program -> Set Program
programsInGroup (Pipes pipes) = go Set.empty
  where
    go result program = if Set.member program result
        then result
        else foldMap (go $ Set.insert program result) $ pipes Map.! program

main :: IO ()
main = do
    pipes <- parse <$> readFile "input.txt"
    putStrLn
        $  "There are "
        <> show (length $ programsInGroup pipes (Program 0))
        <> " programs in the group that contains program ID 0"
