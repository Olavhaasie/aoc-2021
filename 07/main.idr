import Data.List
import Data.List1
import Data.String

import System.File

fuel : List Int -> Int -> Int
fuel [] y = 0
fuel (x :: xs) y = abs (x - y) + fuel xs y

run : String -> IO ()
run s = do let l = catMaybes $ parseInteger {a=Int} <$> (forget $ split (== ',') s)
           let u = (\(c ::: _) => c) <$> (group $ sort $ l)
           let fs = head' $ sort $ fuel l <$> u
           putStrLn $ show fs

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

