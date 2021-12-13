import Data.List
import Data.List1
import Data.String.Parser

import System.File

Coord : Type
Coord = (Nat, Nat)

Dots : Type
Dots = List Coord

data Fold = Up Nat | Left Nat

Show Fold where
  show (Up n) = "fold up " ++ show n
  show (Left n) = "fold left " ++ show n

Input : Type
Input = (Dots, List Fold)

pairParser : Parser Coord
pairParser = do x <- natural
                token ","
                y <- natural
                pure (x, y)

foldParser : Parser Fold
foldParser = do token "fold along"
                (token "y=" *> Up <$> natural) <|>
                (token "x=" *> Left <$> natural)

parser : Parser Input
parser = do coords <- some (pairParser <* spaces)
            spaces
            folds <- some (foldParser <* spaces)
            pure (coords, folds)


fold1 : Fold -> Coord -> Coord
fold1 (Up f)   (x, y) = if y > f
                           then (x, minus f (minus y f))
                           else (x, y)
fold1 (Left f) (x, y) = if x > f
                           then (minus f (minus x f), y)
                           else (x, y)

fold : Fold -> Dots -> Dots
fold f dots = .head <$> (group $ sort $ fold1 f <$> dots)

part1 : Input -> IO String
part1 (dots, []) = pure "No folds"
part1 (dots, (f :: _)) = pure $ show $ length $ fold f dots


printDots : Coord -> Dots -> IO ()
printDots (_, _) [] = pure ()
printDots (lx, ly) ((y, x) :: cs) = 
  do if y > ly
        then putStr $ pack $ replicate (minus y ly) '\n'
        else putStr $ pack $ replicate (minus x (lx + 1)) ' '
     putChar '#'
     printDots (x, y) cs

part2 : Input -> IO String
part2 (dots, fs) = do let r = sort $ (\(x, y) => (y, x)) <$> foldl (flip fold) dots fs
                      printDots (0, 0) r
                      pure ""


main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          Right (a, _) <- pure $ parse parser input
            | Left err => printLn err
          part1 a >>= putStrLn
          part2 a >>= putStrLn

