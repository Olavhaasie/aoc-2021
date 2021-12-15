import Data.Fin
import Data.List
import Data.Nat
import Data.SortedSet as S
import Data.String.Parser
import Data.Vect

import System.File


Node : Type
Node = (Nat, Nat)

-- A Non empty grid
Grid : Nat -> Type
Grid n = Vect (S n) (Vect (S n) Node)

gupdateAt : (Fin (S n), Fin (S n)) -> (Node -> Node) -> Grid n -> Grid n
gupdateAt (x, y) f = updateAt y (updateAt x f)

gindex : (Fin (S n), Fin (S n)) -> Grid n -> Node
gindex (x, y) = index x . index y

inf : Nat
inf = 1000000

parser : Parser (n ** Grid n)
parser = do row <- some digit <* char '\n'
            let n = length row
            Just v <- pure $ toVect n $ (,inf) . finToNat <$> row
              | Nothing => fail "not length"
            Yes ItIsSucc <- pure $ isItSucc n
              | No _ => fail "Row is zero"
            vs <- ntimes (pred n) (((map ((,inf) . finToNat)) <$> ntimes n digit) <* char '\n')
            pure $ MkDPair (pred n) (v :: vs)


decFin : Fin n -> Maybe (Fin n)
decFin FZ = Nothing
decFin (FS x) = Just $ weaken $ x
incFin : {n : Nat} -> Fin n -> Maybe (Fin n)
incFin f = strengthen $ shift 1 f

neighbours : {n : Nat} -> (Fin n, Fin n) -> List (Fin n, Fin n)
neighbours (x, y) = ((, y) <$> catMaybes [decFin x, incFin x]) ++
                    ((x, ) <$> catMaybes [decFin y, incFin y])

lowestRisk' : {n : Nat} -> List (Fin (S n), Fin (S n)) -> Grid n -> Grid n
lowestRisk' [] g = g
lowestRisk' ((x, y) :: queue) g = let (_, d) = gindex (x, y) g
                                      n = filter (\p => let (v, t) = gindex p g in v + d < t) $ neighbours (x, y)
                                      ng = foldl (\g' => \p => gupdateAt p (\(v, _) => (v, v + d)) g') g $ n
                                      nq = queue ++ n in
                                      lowestRisk' nq ng

lowestRisk : {n : Nat} -> Grid n -> Grid n
lowestRisk g = lowestRisk' ((0, 0) :: []) (gupdateAt (0, 0) (mapSnd (const 0)) g)


wrapAdd : Nat -> Nat -> Nat
wrapAdd 0 n = n
wrapAdd (S k) n = if n == 9
                     then wrapAdd k 1
                     else wrapAdd k (S n)

largerRow : Vect (S n) (Nat, Nat) -> Vect (5 * (S n)) (Nat, Nat)
largerRow v = v ++ (concat $ ((\a => mapFst (wrapAdd a) <$> v) <$> [1,2,3,4]))

largerColumn : Vect (S n) (Vect (5 * (S n)) (Nat, Nat)) -> Vect (5 * (S n)) (Vect (5 * (S n)) (Nat, Nat))
largerColumn v = v ++ (concat $ ((\a => (mapFst (wrapAdd a) <$>) <$> v) <$> [1,2,3,4]))

largerGrid : Grid n -> Grid (pred (5 * (S n)))
largerGrid g = let rowextended = largerRow <$> g in
                   largerColumn rowextended

run : String -> IO ()
run s = do Right (MkDPair n g, _) <- pure $ parse parser s
             | Left err => putStrLn $ show err
           let g = lowestRisk $ largerGrid g
           putStrLn $ show $ snd $ gindex (last, last) g

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

