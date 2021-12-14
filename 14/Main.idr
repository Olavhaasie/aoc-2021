import Data.List
import Data.List1
import Data.SortedMap as M
import Data.String.Parser

import System.File


Base : Type
Base = Char

Polymer : Type
Polymer = List1 Base

CountMap : Type
CountMap = SortedMap Base Nat

addCounts : CountMap -> CountMap -> CountMap
addCounts = M.mergeWith (+)

addBase : Base -> CountMap -> CountMap
addBase b = addCounts (M.singleton b 1)

decBase : Base -> CountMap -> CountMap
decBase b m = case M.lookup b m of
                   Just n => insert b (minus n 1) m
                   Nothing => m

fromPair : (Base, Base) -> CountMap
fromPair (b1, b2) = addCounts (M.singleton b1 1) (M.singleton b2 1)

Memoize : Type
Memoize = SortedMap ((Base,Base), Nat) CountMap

Rules : Type
Rules = SortedMap (Base, Base) Base

Input : Type
Input = (Polymer, Rules)


ruleParser : Parser ((Base, Base), Base)
ruleParser = do b1 <- letter
                b2 <- letter
                spaces1
                token "->"
                to <- letter
                pure ((b1, b2), to)

parser : Parser Input
parser = do polymer <- some letter
            Just polymer <- pure $ fromList polymer
              | Nothing => fail "no polymer"
            spaces1
            rules <- some (ruleParser <* spaces)
            pure (polymer, M.fromList rules)

countBases : Polymer -> CountMap
countBases p = M.fromList $ (\l => (l.head, length $ forget l)) <$> (group $ sort $ forget p)

insertNPair : Nat -> (Base, Base) -> Rules -> Memoize -> (CountMap, Memoize)
insertNPair Z p _ m = (fromPair p, m)
insertNPair (S n) (b1, b2) r m = case M.lookup ((b1, b2), (S n)) m of
                                      Nothing => case M.lookup (b1, b2) r of
                                                        Nothing => (fromPair (b1, b2), m)
                                                        (Just b) => let (c1, m1) = insertNPair n (b1, b) r m
                                                                        (c2, m2) = insertNPair n (b, b2) r m1
                                                                        c3 = decBase b $ addCounts c1 c2 in
                                                                        (c3, M.insert ((b1, b2), (S n)) c3 m2)
                                      (Just c1) => (c1, m)

insertN : Nat -> Polymer -> Rules -> Memoize -> CountMap
insertN n (b1 ::: []) r m = M.singleton b1 1
insertN n (b1 ::: (b2 :: p)) r m = let (c1, m1) = (insertNPair n (b1, b2) r m)
                                       c2 = insertN n (b2 ::: p) r m1
                                       c3 = addCounts c1 c2 in
                                       decBase b2 c3

subtractQuantity : CountMap -> Maybe Nat
subtractQuantity m = do let l = sort $ values m
                        high <- last' l
                        low <- head' l
                        pure $ minus high low

part1 : Input -> IO String
part1 (p, r) = do let c = insertN 10 p r M.empty
                  pure $ show $ subtractQuantity c


part2 : Input -> IO String
part2 (p, r) = do let c = insertN 40 p r M.empty
                  pure $ show $ subtractQuantity c


main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          Right (a, _) <- pure $ parse parser input
            | Left err => printLn err
          part1 a >>= putStrLn
          part2 a >>= putStrLn

