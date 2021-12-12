import Data.String.Parser

import System.File

Input : Type
Input = Nat

parser : Parser Input
parser = natural

part1 : Input -> IO String
part1 a = pure "Part 1 not implemented"

part2 : Input -> IO String
part2 a = pure "Part 2 not implemented"

main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          Right (a, _) <- pure $ parse parser input
            | Left err => printLn err
          part1 a >>= putStrLn
          part2 a >>= putStrLn

