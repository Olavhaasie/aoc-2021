import Data.List1
import Data.Nat
import Data.String.Parser

import System.File

data SnailfishNum = Regular Nat | Pair SnailfishNum SnailfishNum

Show SnailfishNum where
  show (Regular k) = show k
  show (Pair x y) = "[" ++ show x ++ "," ++ show y ++ "]"

data ReduceResult = None | Done | Add Nat Nat | AddL Nat | AddR Nat

Input : Type
Input = List1 SnailfishNum

pairParser : Parser a -> Parser (a, a)
pairParser p = do fst <- p
                  skip $ char ','
                  snd <- p
                  pure (fst, snd)

numParser : Parser SnailfishNum
numParser = (char '[' *> (uncurry Pair <$> pairParser numParser) <* char ']') <|>
            Regular <$> natural

parser : Parser Input
parser = do Just l <- fromList <$> some (numParser <* spaces)
              | Nothing => fail "empty list"
            pure l


reduce : SnailfishNum -> SnailfishNum
reduce n = case explode 0 n of
                (r, None) => case split r of
                                  (r, True) => reduce r
                                  (r, False) => r
                (r, _   ) => reduce r
  where
    split : SnailfishNum -> (SnailfishNum, Bool)
    split (Regular k) =
      if k > 9
         then (Pair (Regular $ divNatNZ k 2 SIsNonZero) (Regular $ divCeilNZ k 2 SIsNonZero), True)
         else (Regular k, False)
    split (Pair x y) = case split x of
                            (z, True) => (Pair z y, True)
                            (z, False) => let (w, b) = split y in
                                              (Pair z w, b)

    addLeftMost : Nat -> SnailfishNum -> SnailfishNum
    addLeftMost n (Regular k) = Regular $ n + k
    addLeftMost n (Pair x y) = Pair (addLeftMost n x) y

    addRightMost : Nat -> SnailfishNum -> SnailfishNum
    addRightMost n (Regular k) = Regular $ n + k
    addRightMost n (Pair x y) = Pair x (addRightMost n y)

    explode : Nat -> SnailfishNum -> (SnailfishNum, ReduceResult)
    explode _ (Regular k) = (Regular k, None)
    explode 4 (Pair (Regular k) (Regular j)) = (Regular 0, Add k j)
    explode d (Pair x y) = case explode (min (d + 1) 4) x of
                                (z, Done) => (Pair z y, Done)
                                (z, (Add k j)) => (Pair z (addLeftMost j y), AddL k)
                                (z, (AddL k)) => (Pair z y, AddL k)
                                (z, (AddR j)) => (Pair z (addLeftMost j y), Done)
                                (z, None) => case explode (min (d + 1) 4) y of
                                                  (w, (Add k j)) => (Pair (addRightMost k z) w, AddR j)
                                                  (w, (AddL k)) => (Pair (addRightMost k z) w, Done)
                                                  (w, r) => (Pair z w, r)

add : SnailfishNum -> SnailfishNum -> SnailfishNum
add n1 n2 = reduce $ Pair n1 n2

magnitude : SnailfishNum -> Nat
magnitude (Regular n) = n
magnitude (Pair x y) = 3 * (magnitude x) + 2 * (magnitude y)

part1 : Input -> IO String
part1 = pure . show . magnitude . foldl1 add

part2 : Input -> IO String
part2 a = pure "Part 2 not implemented"

main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          Right (a, _) <- pure $ parse parser input
            | Left err => printLn err
          part1 a >>= putStrLn
          part2 a >>= putStrLn

