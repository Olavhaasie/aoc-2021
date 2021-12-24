import Data.Nat
import Data.Fin
import Data.Fin.Extra
import Data.Vect
import Data.List
import Data.List1

inf : Nat
inf = 1000000

data Amphipod = A | B | C | D

Eq Amphipod where
  A == A = True
  B == B = True
  C == C = True
  D == D = True
  _ == _ = False

energy : Amphipod -> Nat
energy A = 1
energy B = 10
energy C = 100
energy D = 1000

roomOf : Amphipod -> Fin 4
roomOf A = 0
roomOf B = 1
roomOf C = 2
roomOf D = 3

Room : Nat -> Type
Room n = Vect n (Maybe Amphipod)

roomToHall : Fin 4 -> Fin 11
roomToHall 0 = 2
roomToHall 1 = 4
roomToHall 2 = 6
roomToHall 3 = 8

record State (s : Nat) where
  constructor MkState
  hall : Room 11
  rooms : Vect 4 (Room s)
  energy : Nat

isPod : Amphipod -> Maybe Amphipod -> Bool
isPod a Nothing = False
isPod a (Just b) = a == b

isSolved : State n -> Bool
isSolved (MkState hall [r1,r2,r3,r4] _) =
  all isNothing hall &&
  all (isPod A) r1 &&
  all (isPod B) r2 &&
  all (isPod C) r3 &&
  all (isPod D) r4

validPlaces : Vect 7 (Fin 11)
validPlaces = [0,1,3,5,7,9,10]

moveToHall : State n -> List (State n)
moveToHall s = []

moveToRoom : {n : Nat} -> State n -> List (State n)
moveToRoom s = catMaybes $ toList $ moveToRoom' s <$> validPlaces
  where
    pathToRoom : State n -> Fin 11 -> Fin 4 -> Maybe Nat
    pathToRoom s from r = do let to = finToNat $ roomToHall r
                             let from = finToNat $ from
                             let (min, max) = if from > to
                                                 then (to, from)
                                                 else (from + 1, to + 1)
                             let dist = minus max min
                             if any isJust $ take dist $ drop min $ toList s.hall
                                then Nothing
                                else Just dist

    moveInRoom : State n -> Fin 11 -> Fin 4 -> Maybe (Fin n)
    moveInRoom s h r = do let room = index r s.rooms
                          if all ((==) r . roomOf) $ catMaybes $ toList room
                             then do map invFin $ findIndex isNothing $ reverse $ room
                             else Nothing

    moveToRoom' : State n -> Fin 11 -> Maybe (State n)
    moveToRoom' s p = do a <- index p s.hall
                         let r = roomOf a
                         toRoom <- pathToRoom s p r
                         inRoom <- moveInRoom s p r
                         let en = energy a
                         pure $ {
                           hall $= replaceAt p Nothing,
                           rooms $= updateAt r (replaceAt inRoom (Just a)),
                           energy $= (+) (en * (toRoom + (finToNat inRoom) + 1))
                         } s

solve : {n : Nat} -> Nat -> State n -> Nat
solve m s = if s.energy > m
                 then m
                 else if isSolved s
                         then s.energy
                         else let m = foldl solve m (moveToHall s) in
                                  foldl solve m (moveToRoom s)

input1 : Vect 4 (Room 2)
input1 = [
  [Just D, Just C],
  [Just B, Just A],
  [Just C, Just D],
  [Just A, Just B]
  ]

part1 : State 2 -> IO String
part1 a = pure $ show $ solve inf a

input2 : Vect 4 (Room 4)
input2 = [
  [Just D, Just D, Just D, Just C],
  [Just B, Just C, Just B, Just A],
  [Just C, Just B, Just A, Just D],
  [Just A, Just A, Just C, Just B]
  ]

part2 : State 4 -> IO String
part2 a = pure $ show $ solve inf a

main : IO ()
main = do let s1 = MkState (replicate 11 Nothing) input1 0
          part1 s1 >>= putStrLn
          let s2 = MkState (replicate 11 Nothing) input2 0
          part2 s2 >>= putStrLn

