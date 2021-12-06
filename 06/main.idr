import Data.List
import Data.List1
import Data.String

import System.File

simulate1 : Nat -> Nat -> Nat
simulate1 0 c = 1
simulate1 (S n) 0 = simulate1 n 6 + simulate1 n 8
simulate1 (S n) (S c) = simulate1 n c

run : String -> IO ()
run s = do let l = group $ sort $ catMaybes $ parsePositive {a=Nat} <$> (forget $ split (== ',') s)
           let n = sum $ (\l@(c ::: _) => (length $ forget $ l) * simulate1 256 c) <$> l
           putStrLn $ show n

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

