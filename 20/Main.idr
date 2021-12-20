import Data.Binary
import Data.Binary.Digit
import Data.Fin
import Data.List
import Data.List1
import Data.Nat
import Data.Vect
import Data.String
import Data.String.Parser

import System.File

Show Digit where
  show I = "1"
  show O = "0"

Algorithm : Type
Algorithm = Vect 512 Bool

Image : Nat -> Type
Image n = Vect n (Vect n Bool)

showImg : Image n -> String
showImg n = fastAppend $ intersperse "\n" $ toList $ (\r => pack $ toList $ (\b => if b then '#' else '.') <$> r) <$> n

Input : Nat -> Type
Input n = (Algorithm, Image n)

pixelParser : Parser Bool
pixelParser = char '.' $> False <|> char '#' $> True

parser : Parser (n ** Input n)
parser = do alg <- ntimes 512 pixelParser
            spaces
            row <- some pixelParser <* char '\n'
            let n = length row
            Just v <- pure $ toVect n row
              | Nothing => fail "not length"
            Yes ItIsSucc <- pure $ isItSucc n
              | No _ => fail "Row is zero"
            vs <- ntimes (pred n) ((ntimes n pixelParser) <* char '\n')
            pure $ MkDPair n (alg, v :: vs)

grow : Bool -> (n : Nat) -> Image n -> Image (4 + n)
grow d n img = let gr = (flip snoc) d . (flip snoc) d . (::) d . (::) d <$> img
                   emptyrow = replicate (4 + n) d in
                   (flip snoc) emptyrow $ (flip snoc) emptyrow $ (::) emptyrow $ (::) emptyrow $ gr


shrink : {n : Nat} -> Image (4 + n) -> Image (2 + n)
shrink = map (init . tail) . init . tail

decFin : Fin n -> Fin (S n)
decFin FZ = FZ
decFin (FS x) = weaken $ weaken x

neighbours : Fin n -> Fin n -> List (Fin (S n), Fin (S n))
neighbours x y = let xs = [ decFin x, weaken x, shift 1 x ]
                     ys = [ decFin y, weaken y, shift 1 y ] in
                     join $ (\y => (,y) <$> xs) <$> ys

getNumber : Image n -> List (Fin n, Fin n) -> Nat
getNumber img l = toNat $ reverse $ (\b => if b then I else O) <$> (\(x, y) => index x $ index y img) <$> l

lookupAlg : Algorithm -> Nat -> Bool
lookupAlg alg n = case natToFin n 512 of
                       Nothing => False
                       (Just i) => index i alg

enhance : {n : Nat} -> Algorithm -> ((k : Nat) -> Image k -> Image (4 + k)) -> Image n -> Image (2 + n)
enhance alg g img = let img' = g n img
                        indices = map (shift 1) $ forget $ allFins (S n)
                        positions = join $ (\x => (x,) <$> indices) <$> indices 
                        enhanced = foldl (\i => \(x, y) => let k = getNumber img' $ neighbours x y
                                                               b = lookupAlg alg k in
                                                               updateAt (weaken y) (replaceAt (weaken x) b) i) img' positions in
                        shrink enhanced

enhanceN : {n : Nat} -> Nat -> Algorithm -> (Bool -> (k : Nat) -> Image k -> Image (4 + k)) -> Image n -> (m ** Image m)
enhanceN 0 alg g img = MkDPair n img
enhanceN (S s) alg g img = let en = enhance alg (g ((modNatNZ (S s) 2 SIsNonZero) == 1)) img in enhanceN {n = (S (S n))} s alg g en

countLit : Image n -> Nat
countLit = sum . map (count id)

part1 : {n : Nat} -> Input n -> IO String
part1 (alg, img) = pure $ show $ countLit $ enhance alg (grow True) $ enhance alg (grow False) img

part2 : {n : Nat} -> Input n -> IO String
part2 (alg, img) = let MkDPair _ enn = enhanceN 50 alg grow img in
                       pure $ show $ countLit $ enn

main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          Right (MkDPair n a, _) <- pure $ parse parser input
            | Left err => printLn err
          part1 a >>= putStrLn
          part2 a >>= putStrLn

