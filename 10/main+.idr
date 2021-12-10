import Data.Either
import Data.List
import Data.Maybe
import Data.Nat
import Data.String

import System.File

data Bracket = P | S | B | L
Eq Bracket where
  P == P = True
  S == S = True
  B == B = True
  L == L = True
  _ == _ = False
data Chunk = Open Bracket | Close Bracket

findMissing : List Bracket -> List Chunk -> List Bracket
findMissing s [] = s
findMissing s ((Open x) :: xs) = findMissing (x :: s)  xs
findMissing [] ((Close x) :: xs) = []
findMissing (y :: ys) ((Close x) :: xs) = if x == y
                                       then findMissing ys xs
                                       else []

parseChunk : Char -> Maybe Chunk
parseChunk '(' = Just $ Open P
parseChunk ')' = Just $ Close P
parseChunk '[' = Just $ Open S
parseChunk ']' = Just $ Close S
parseChunk '{' = Just $ Open B
parseChunk '}' = Just $ Close B
parseChunk '<' = Just $ Open L
parseChunk '>' = Just $ Close L
parseChunk _ = Nothing
parseChunks : List Char -> List (Maybe Chunk)
parseChunks l = parseChunk <$> l

score : Bracket -> Nat
score P = 1
score S = 2
score B = 3
score L = 4

run : String -> IO ()
run s = do let l = filter (\l => length l > 0) $ findMissing [] . catMaybes . parseChunks . unpack <$> lines s
           let s = sort $ (\bs => foldl (\a => \n => 5 * a + n ) 0 $ score <$> bs) <$> l
           let h = (div (length s) 2) + 1
           putStrLn $ show $ head' $ reverse $ take h s

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

