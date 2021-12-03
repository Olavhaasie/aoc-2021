import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Data.Binary
import Data.Binary.Digit

import System.File

significant: List Nat -> String -> List Nat
significant l s = (\(i,c) => if c == '1' then i + 1 else i)
                  <$> zip l (unpack s)

gamma : Nat -> List Nat -> Nat
gamma s l = toNat $ reverse $ (\i => if i + i > s then I else O) <$> l
epsilon : Nat -> List Nat -> Nat
epsilon s l = toNat $ reverse $ (\i => if i + i > s then O else I) <$> l

run : String -> IO ()
run s = do let lines = lines $ trim $ s
           let s = foldl significant (replicate 12 0) $ lines
           let gamma = gamma (length lines) s
           let epsilon = epsilon (length lines) s
           putStrLn $ show $ gamma * epsilon

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

