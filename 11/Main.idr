import Data.Either
import Data.Fin
import Data.List
import Data.List1
import Data.Nat
import Data.Maybe
import Data.Stream
import Data.String
import Data.String.Parser
import Data.Vect

import System.File

Energy : Type
Energy = Fin 10

data Flash = Flashed | E Energy

incEnergy : Energy -> Flash
incEnergy e = maybe Flashed E $ strengthen $ shift 1 e

isFlash : Flash -> Bool
isFlash Flashed = True
isFlash _ = False

resetFlash : Flash -> Energy
resetFlash Flashed = 0
resetFlash (E e) = e

-- A Non empty grid
Grid : Nat -> Type -> Type
Grid n a = Vect (S n) (Vect (S n) a)


parser : Parser (n ** Grid n Energy)
parser = do row <- some digit <* char '\n'
            let n = length row
            Just v <- pure $ toVect n row
              | Nothing => fail "not length"
            Yes ItIsSucc <- pure $ isItSucc n
              | No _ => fail "Row is zero"
            vs <- ntimes (pred n) ((ntimes n digit) <* char '\n')
            pure $ MkDPair (pred n) (v :: vs)

decFin : Fin n -> Maybe (Fin n)
decFin FZ = Nothing
decFin (FS x) = Just $ weaken $ x
incFin : {n : Nat} -> Fin n -> Maybe (Fin n)
incFin f = strengthen $ shift 1 f
neighbours1 : {n : Nat} -> Fin n -> List (Fin n)
neighbours1 x = catMaybes $ [ decFin x, Just x, incFin x ]

neighbours : {n : Nat} -> Fin (S n) -> Fin (S n) -> List (Fin (S n), Fin (S n))
neighbours x y = delete (x, y) $ join $ (\x => (x,) <$> (neighbours1 y)) <$> neighbours1 x

incNeighbours : {n : Nat} -> List (Fin (S n), Fin (S n)) -> Grid n Flash -> Grid n Flash
incNeighbours [] g = g
incNeighbours ((x, y) :: ps) g = case index x $ index y $ g of
                                      Flashed => incNeighbours ps g
                                      (E e) => case incEnergy e of
                                                    Flashed => incNeighbours (neighbours x y ++ ps) (updateAt y (replaceAt x Flashed) g)
                                                    en@(E _) => incNeighbours ps (updateAt y (replaceAt x en) g)

step : {n : Nat} -> Grid n Energy -> Grid n Flash
step {n} g = let gf = (E <$>) <$> g
                 ps = join $ (\y => (, y) <$> (forget $ allFins n)) <$> (forget $ allFins n) in
                 incNeighbours ps gf

reset : Grid n Flash -> (Nat, Grid n Energy)
reset g = (sum $ count isFlash <$> g, (resetFlash <$>) <$> g)

simulate : {n : Nat} -> Nat -> Grid n Energy -> Nat
simulate 0 = const 0
simulate (S s) = (\(f, g) => f + simulate s g) . reset . step 

allFlash : {n : Nat} -> Nat -> Grid n Energy -> Nat
allFlash s g = let (c, gf) = reset $ step g in
                 if c == (S n) * (S n)
                    then S s
                    else allFlash (S s) gf

run : String -> IO ()
run s = do Right (MkDPair n g, _) <- pure $ parse parser s
             | Left err => putStrLn $ show err
           putStrLn $ show $ simulate 195 g
           putStrLn $ show $ allFlash 0 g

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

