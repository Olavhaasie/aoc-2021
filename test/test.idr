import Data.Either
import Data.List
import Data.Maybe
import Data.String

import System.File

findSum3 : Int -> List Int -> Maybe Int
findSum3 x [] = Nothing
findSum3 x (y :: xs) = if x + y == 2020
                          then Just y
                          else findSum3 x xs

findSum : Int -> List Int -> Maybe Int
findSum x [] = Nothing
findSum x (y :: xs) = let z = x + y
                      in if z > 2020
                            then findSum x xs
                            else ((\w => w * x * y) <$> findSum3 z xs) <|> findSum x xs

findProduct : List Int -> Maybe Int
findProduct [] = Nothing
findProduct (x :: xs) = findSum x xs <|> findProduct xs

parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

run : String -> IO ()
run s = do let l = parseLines parseInteger {a=Int} s
           let r = findProduct l
           putStrLn $ show r

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

