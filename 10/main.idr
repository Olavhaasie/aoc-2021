import Data.Either
import Data.List
import Data.Maybe
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

findCorrupted : List Bracket -> List Chunk -> Maybe Bracket
findCorrupted s [] = Nothing
findCorrupted s ((Open x) :: xs) = findCorrupted (x :: s)  xs
findCorrupted [] ((Close x) :: xs) = Just x
findCorrupted (y :: ys) ((Close x) :: xs) = if x == y
                                       then findCorrupted ys xs
                                       else Just x

score : Bracket -> Nat
score P = 3
score S = 57
score B = 1197
score L = 25137

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

run : String -> IO ()
run s = do let l = findCorrupted [] . catMaybes . parseChunks . unpack <$> lines s
           let s = sum $ catMaybes $ (score <$>) <$> l
           putStrLn $ show s

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

