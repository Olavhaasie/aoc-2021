import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Data.Binary
import Data.Binary.Digit

import System.File

Eq Digit where
  I == I = True
  O == O = True
  I == O = False
  O == I = False

lookupMaybe : Nat -> List a -> Maybe a
lookupMaybe _ [] = Nothing
lookupMaybe Z (x :: _) = Just x
lookupMaybe (S n) (_ :: l) = lookupMaybe n l

significant : (Nat, Nat) -> Bin -> (Nat, Nat)
significant (n, c) b = case lookupMaybe n b of
                            Just d => case d of
                                           I => (n, c + 1)
                                           O => (n, c)
                            Nothing => (n, c)

toBin : String -> Bin
toBin s = (\c => if c == '1' then I else O) <$> unpack s

oxygen2 : Nat -> List Bin -> Bin
oxygen2 n [] = []
oxygen2 n (b :: []) = b
oxygen2 n l@(_ :: _ :: _) = let len = length l
                                (_, c) = foldl significant (n, 0) l
                                d = if c + c >= len then I else O in
                                oxygen2 (n + 1) $ filter (\b => case lookupMaybe n b of
                                                                     Just d2 => d == d2
                                                                     Nothing => False) $ l

oxygen : List Bin -> Bin
oxygen l = oxygen2 0 l

scrubber2 : Nat -> List Bin -> Bin
scrubber2 n [] = []
scrubber2 n (b :: []) = b
scrubber2 n l@(_ :: _ :: _) = let len = length l
                                  (_, c) = foldl significant (n, 0) l
                                  d = if c + c >= len then O else I in
                                  scrubber2 (n + 1) $ filter (\b => case lookupMaybe n b of
                                                                         Just d2 => d == d2
                                                                         Nothing => False) $ l

scrubber : List Bin -> Bin
scrubber l = scrubber2 0 l

run : String -> IO ()
run s = do let lines = toBin <$> (lines $ trim $ s)
           let o = toNat $ reverse $ oxygen $ lines
           let s = toNat $ reverse $ scrubber $ lines
           putStrLn $ show $ s * o

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

