import Data.Binary
import Data.Binary.Digit
import Data.List
import Data.Nat
import Data.String
import Data.String.Parser
import Data.Vect

import System.File

shexMap : List (Char, String)
shexMap = [
  ('0', "0000"),
  ('1', "0001"),
  ('2', "0010"),
  ('3', "0011"),
  ('4', "0100"),
  ('5', "0101"),
  ('6', "0110"),
  ('7', "0111"),
  ('8', "1000"),
  ('9', "1001"),
  ('A', "1010"),
  ('B', "1011"),
  ('C', "1100"),
  ('D', "1101"),
  ('E', "1110"),
  ('F', "1111")
  ]

Input : Type
Input = Bin

binToNat : Vect n Digit -> Nat
binToNat = toNat . reverse . toList

digitParser : Parser Digit
digitParser = char '0' $> O <|> char '1' $> I

numParser : Nat -> Parser Nat
numParser n = binToNat <$> ntimes n digitParser

litParser : Parser Bin
litParser = do d <- digitParser
               case d of
                    O => toList <$> ntimes 4 digitParser
                    I => do b <- toList <$> ntimes 4 digitParser
                            ((++) b) <$> litParser

doExp : Nat -> List Nat -> Maybe Nat
doExp 0 l = pure $ foldl (+) 0 l
doExp 1 l = pure $ foldl (*) 1 l
doExp 2 (x :: xs) = pure $ foldl min x xs
doExp 3 (x :: xs) = pure $ foldl max x xs
doExp 5 (x :: y :: []) = pure $ if x > y then 1 else 0
doExp 6 (x :: y :: []) = pure $ if x < y then 1 else 0
doExp 7 (x :: y :: []) = pure $ if x == y then 1 else 0
doExp _ _ = Nothing

expParser : Parser Nat
expParser = do version <- numParser 3
               type <- numParser 3
               if type == 4
                  then do lit <- toNat . reverse <$> litParser
                          pure $ lit
                  else do ltype <- numParser 1
                          if ltype == 1
                             then do num <- numParser 11
                                     vs <- ntimes num expParser
                                     maybe (fail "Unknown op") pure $ doExp type $ toList vs
                             else do len <- numParser 15
                                     sub <- pack . toList <$> ntimes len alphaNum
                                     Right (vs, _) <- pure $ parse (some expParser) sub
                                       | Left err => fail err
                                     maybe (fail "Unknown op") pure $ doExp type $ toList vs

part2 : String -> IO String
part2 a = do Right (v, _) <- pure $ parse expParser a
               | Left err => pure err
             pure $ show v
main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          let a = fastAppend $ catMaybes $ (flip lookup $ shexMap) <$> unpack input
          part2 a >>= putStrLn

