import Data.Either
import Data.List
import Data.List1
import Data.Maybe
import Data.String

import System.File

data BingoNum = M Nat | U Nat

Show BingoNum where
  show (M k) = "m " ++ show k
  show (U k) = "u " ++ show k

Card : Type
Card = List (List BingoNum)

parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

getNumbers : List String -> Maybe (List Nat, List String)
getNumbers [] = Nothing
getNumbers (x :: xs) = Just (catMaybes $ parsePositive <$> (forget $ split (== ',') x),  xs)

parseCard : List String -> Card
parseCard l = (\s => U <$> (catMaybes $ parsePositive <$> (forget $ split (== ' ') s))) <$> l

parseCards : List String -> List Card
parseCards [] = []
parseCards (_ :: xs) = parseCard (take 5 xs) :: parseCards (drop 5 xs)

isMarked : BingoNum -> Bool
isMarked (M _) = True
isMarked (U _) = False

winRow : Card -> Bool
winRow [] = False
winRow (x :: xs) = all isMarked x || winRow xs

winColumn : Card -> Bool
winColumn c = any id $ foldl (\a => \bs => (\(b1, b2) => b1 && b2) <$> zip a (isMarked <$> bs)) (replicate 5 True) c

wins : Card -> Bool
wins c = winRow c || winColumn c

drawNumber : Nat -> Card -> Card
drawNumber n [] = []
drawNumber n (x :: xs) = ((\d => case d of
                                      M m => M m
                                      U u => if u == n
                                                then M u
                                                else U u) <$> x) :: drawNumber n xs

drawNumbers : List Nat -> List Card -> Maybe (Card, Nat)
drawNumbers [] cs = Nothing
drawNumbers (x :: xs) [] = Nothing
drawNumbers (x :: xs) (c :: []) = let nc = drawNumber x c in
                                      if wins nc
                                         then Just (nc, x)
                                         else drawNumbers xs (nc :: [])
drawNumbers (x :: xs) cs = let ncs = drawNumber x <$> cs
                               losing = filter (not . wins) ncs in
                               drawNumbers xs losing

scoreOf : Card -> Nat
scoreOf [] = 0
scoreOf (x :: xs) = (foldl (\a => \b => case b of
                                             M n => a
                                             U n => n + a) 0 x) + scoreOf xs

run : String -> IO ()
run s = do let lines = lines s
           Just (ns, lines) <- pure $ getNumbers lines
             | Nothing => putStrLn $ "err"
           let cards = parseCards lines
           Just (win, n) <- pure $ drawNumbers ns cards
             | Nothing => putStrLn $ "no win"
           let score = n * scoreOf win
           putStrLn $ show $ score

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

