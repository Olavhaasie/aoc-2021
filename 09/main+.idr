import Data.Either
import Data.List
import Data.Maybe
import Data.String

import System.File

data LowPoint = L Nat | N Nat

get : LowPoint -> Nat
get (L k) = k
get (N k) = k

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

HMap : Type
HMap = List ((Nat, Nat), Nat)

toMap : (Nat, Nat) -> List (List LowPoint) -> (HMap, List (Nat, Nat))
toMap (x, y) [] = ([], [])
toMap (x, y) ([] :: xs) = toMap (0, y + 1) xs
toMap (x, y) (((L k) :: ys) :: xs) = let (m, ls) = toMap (x + 1, y) (ys :: xs) in
                                       (((x, y), k) :: m, (x,y) :: ls)
toMap (x, y) (((N k) :: ys) :: xs) = let (m, ls) = toMap (x + 1, y) (ys :: xs) in
                                       (((x, y), k) :: m, ls)

basinFrom : HMap -> List (Nat, Nat) -> (Nat, Nat) -> List (Nat, Nat)
basinFrom m p (x, y) = if elem (x, y) p
                          then p
                          else case lookup (x, y) m of
                                    Just k => if k < 9
                                                 then let p0 = (x, y) :: p
                                                          p1 = basinFrom m p0 (x + 1, y)
                                                          p2 = basinFrom m p1 (x, y + 1)
                                                          p3 = basinFrom m p2 (minus x 1, y) in
                                                          basinFrom m p3 (x, minus y 1)
                                                 else p
                                    Nothing => p

run : String -> IO ()
run s = do let l = (\l => catMaybes $ parsePositive {a=Nat} . singleton <$> unpack l) <$> lines s
           let r = lowPointsCol (replicate 100 10) $ lowPointsRow 10 <$> l
           let (m, ls) = toMap (0,0) r
           let c = product $ take 3 $ reverse $ sort $ (length . basinFrom m []) <$> ls
           putStrLn $ show c

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

