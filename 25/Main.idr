import Data.Vect as V
import Data.Vect.Extra
import Data.Nat
import Data.List as L
import Data.String.Parser

import System.File

data Cucumber = H | V

isH : Maybe Cucumber -> Bool
isH (Just H) = True
isH _ = False

isV : Maybe Cucumber -> Bool
isV (Just V) = True
isV _ = False

Grid : Nat -> Nat -> Type
Grid x y = Vect (S y) (Vect (S x) (Maybe Cucumber))

cucumberParser : Parser (Maybe Cucumber)
cucumberParser = char '>' $> Just H <|> char 'v' $> Just V <|> char '.' $> Nothing

parser : Parser (x ** y ** Grid x y)
parser = do row <- some cucumberParser <* char '\n'
            let x = length row
            Just v <- pure $ toVect x row
              | Nothing => fail "not length"
            Yes ItIsSucc <- pure $ isItSucc x
              | No _ => fail "Row is zero"
            cs <- some ((ntimes x cucumberParser) <* char '\n')
            let y = length cs
            Just cs <- pure $ toVect y cs
              | Nothing => fail "not column"
            pure $ (_ ** (_ ** v :: cs))

Move : Nat -> Nat -> Type
Move x y = ((Fin (S x), Fin (S y)), (Fin (S x), Fin (S y)))

addWrap : {n : Nat} -> Fin (S n) -> Fin (S n)
addWrap f = case strengthen $ shift 1 f of
               Nothing => 0
               (Just f) => f

hMoves : {x : Nat} -> {y : Nat} -> Grid x y -> List (Move x y)
hMoves g = filter (\(_,(x,y)) => isNothing $ index x $ index y g) $ map (\(x,y,_) => ((x, y), (addWrap x, y))) $ toList $ join $ toList $ (\(y,xs) => filter (isH . snd . snd) $ toList $ mapWithPos (,y,) $ xs) <$> mapWithPos (,) g

vMoves : {x : Nat} -> {y : Nat} -> Grid x y -> List (Move x y)
vMoves g = filter (\(_,(x,y)) => isNothing $ index x $ index y g) $ map (\(x,y,_) => ((x, y), (x, addWrap y))) $ toList $ join $ toList $ (\(y,xs) => filter (isV . snd . snd) $ toList $ mapWithPos (,y,) $ xs) <$> mapWithPos (,) g

steps : {x : Nat} -> {y : Nat} -> Grid x y -> Nat
steps g = steps' 1 g
  where
    doMoves : Grid x y -> List (Move x y) -> Grid x y
    doMoves g [] = g
    doMoves g (((x1, y1), (x2, y2)) :: ms) = let c = index x1 $ index y1 g 
                                                 g1 = updateAt y1 (replaceAt x1 Nothing) g
                                                 g2 = updateAt y2 (replaceAt x2 c) g1 in
                                                 doMoves g2 ms

    steps' : Nat -> Grid x y -> Nat
    steps' n g = case hMoves g of
                      [] => case vMoves g of
                                 [] => n
                                 ms => steps' (S n) (doMoves g ms)
                      ms => let g' = doMoves g ms in
                                case vMoves g' of
                                     [] => steps' (S n) g'
                                     ms => steps' (S n) (doMoves g' ms)


part1 : {n : Nat} -> {m : Nat} -> Grid n m -> IO String
part1 a = pure $ show $ steps a

main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          Right (MkDPair n (MkDPair m a), _) <- pure $ parse parser input
            | Left err => printLn err
          part1 a >>= putStrLn

