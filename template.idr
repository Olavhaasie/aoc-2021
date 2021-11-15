import Data.Either
import Data.List
import Data.Maybe
import Data.String

import System.File


parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

run : String -> IO ()
run s = do let l = parseLines parseInteger {a=Int} s
           putStrLn $ show l

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

