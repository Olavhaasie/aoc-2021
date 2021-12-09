import Data.Either
import Data.List
import Data.Maybe
import Data.String

import System.File

data LowPoint = L Nat | N Nat

Show LowPoint where
  show (L k) = "L " ++ show k
  show (N k) = "N " ++ show k

get : LowPoint -> Nat
get (L k) = k
get (N k) = k

isLow : LowPoint -> Bool
isLow (L k) = True
isLow (N k) = False

parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

lowPointsRow : Nat -> List Nat -> List LowPoint
lowPointsRow n [] = []
lowPointsRow n (x :: []) = if x < n then [L x] else [N x]
lowPointsRow n (x :: (y :: xs)) = (if x < n && x < y then L x else N x) :: lowPointsRow x (y :: xs)

comp : Nat -> LowPoint -> LowPoint
comp p (L k) = if k < p then L k else N k
comp _ (N k) = N k

comp3 : Nat -> LowPoint -> Nat -> LowPoint
comp3 p (L k) n = if k < n && k < p then L k else N k
comp3 _ (N k) _ = N k

lowPointsCol : List Nat -> List (List LowPoint) -> List (List LowPoint)
lowPointsCol prev [] = []
lowPointsCol prev (row :: []) = [ (\(p, r) => comp p r) <$> zip prev row ]
lowPointsCol prev (row :: (next :: rows)) = ((\(p, r, n) => comp3 p r (get n)) <$> zip3 prev row next) :: lowPointsCol (get <$> row) (next :: rows)

run : String -> IO ()
run s = do let l = (\l => catMaybes $ parsePositive {a=Nat} . singleton <$> unpack l) <$> lines s
           let r = lowPointsCol (replicate 100 10) $ lowPointsRow 10 <$> l
           let s = sum $ S . get <$> (filter isLow $ (join r))
           putStrLn $ show $ s

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

