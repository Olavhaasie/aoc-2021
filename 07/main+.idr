import Data.List
import Data.List1
import Data.String

import System.File

cost : Nat -> Nat
cost 0 = 0
cost n@(S k) = n + cost k

fuel : List Nat -> Nat -> Nat
fuel [] y = 0
fuel (x :: xs) y = (cost (if x > y then minus x y else minus y x)) + fuel xs y

upTo : Nat -> List Nat
upTo 0 = 0 :: []
upTo (S k) = S k :: upTo k

run : String -> IO ()
run s = do let l = catMaybes $ parsePositive {a=Nat} <$> (forget $ split (== ',') s)
           Just u <- pure $ head' $ reverse $ (\(c ::: _) => c) <$> (group $ sort $ l)
             | Nothing => putStrLn "no upper bound"
           let fs = head' $ sort $ fuel l <$> (upTo u)
           putStrLn $ show fs

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

