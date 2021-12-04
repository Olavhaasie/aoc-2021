import Data.Either
import Data.List
import Data.Maybe
import Data.String

import System.File

increases : List Int -> Int
increases [] = 0
increases (x :: []) = 0
increases (x :: (y :: xs)) = (if x < y then 1 else 0) + increases (y ::xs)

parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

run : String -> IO ()
run s = do let l = parseLines parseInteger {a=Int} s
           putStrLn $ show $ increases l

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

