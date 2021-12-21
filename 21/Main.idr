import Data.List
import Data.Nat
import Data.SortedMap
import Control.Monad.State
import Control.Monad.State.State
import Control.Monad.State.Interface

record Player where
  constructor MkPlayer
  score : Nat
  pos : Nat

Eq Player where
  p1 == p2 = p1.score == p2.score && p1.pos == p2.pos

Ord Player where
  compare p1 p2 = compare (p1.score, p1.pos) (p2.score, p2.pos)

data Turn = P1 | P2

Eq Turn where
  P1 == P1 = True
  P2 == P2 = True
  _  == _  = False

Ord Turn where
  compare P1 P1 = EQ
  compare P1 P2 = LT
  compare P2 P1 = GT
  compare P2 P2 = EQ

next : Turn -> Turn
next P1 = P2
next P2 = P1

record Game where
  constructor MkGame
  player1, player2 : Player
  turn : Turn
  rolls : Nat

fromStart : Nat -> Nat -> Game
fromStart p1 p2 = MkGame (MkPlayer 0 p1) (MkPlayer 0 p2) P1 0

score : Game -> Nat
score (MkGame p1 p2 _ rolls) = (*) rolls $ max p1.score  p2.score

rollDet : State Game Nat
rollDet = do rolls <- gets (.rolls)
             modify { rolls $= S }
             pure $ 1 + (modNatNZ rolls 100 SIsNonZero)

movePlayer : Nat -> Player -> Player
movePlayer m (MkPlayer score pos) = let npos = (modNatNZ (pos + m) 10 SIsNonZero) in
                                         MkPlayer (score + npos + 1) npos

playDet : State Game ()
playDet = do rolls <- sequence $ replicate 3 rollDet
             let r = sum rolls
             turn <- gets (.turn)
             case turn of
                  P1 => modify { player1 $= movePlayer r }
                  P2 => modify { player2 $= movePlayer r }
             s1 <- gets (.player1.score)
             s2 <- gets (.player2.score)
             if s1 >= 1000 || s2 >= 1000
                then pure ()
                else modify { turn $= next } *> playDet

part1 : Game -> IO String
part1 a = do let g = execState a playDet
             pure $ show $ score g


Memoize : Type
Memoize = SortedMap (Player, Player, Turn) (Nat, Nat)

addPair : Num a => (a, a) -> (a, a) -> (a, a)
addPair (x, y) (z, w) = (x + z, y + w)

wins : Player -> Bool
wins (MkPlayer score pos) = score >= 21

playDirac : Nat -> Memoize -> Game -> ((Nat, Nat), Memoize)
playDirac _ m (MkGame p1 p2 turn 0) = case lookup (p1, p2, turn) m of
                                           (Just w) => (w, m)
                                           Nothing => let (w, m) = foldl (\(w, m) => \d => mapFst (addPair w) $ playDirac d m (MkGame p1 p2 turn 1)) ((0, 0), m) $ [1,2,3] in
                                                          (w, insert (p1, p2, turn) w m)
playDirac s m (MkGame p1 p2 P1 3) = let p1' = movePlayer s p1 in
                                        if wins p1'
                                           then ((1, 0), m)
                                           else playDirac 0 m (MkGame p1' p2 P2 0)
playDirac s m (MkGame p1 p2 P2 3) = let p2' = movePlayer s p2 in
                                        if wins p2'
                                           then ((0, 1), m)
                                           else playDirac 0 m (MkGame p1 p2' P1 0)
playDirac s m (MkGame p1 p2 turn r) = foldl (\(w, m) => \d => mapFst (addPair w) $ playDirac (s + d) m (MkGame p1 p2 turn (1 + r))) ((0, 0), m) $ [1,2,3]

part2 : Game -> IO String
part2 a = do let (wins, _) = playDirac 0 empty a
             pure $ show $ (uncurry max) wins


main : IO ()
main = do let g = fromStart 2 9
          part1 g >>= putStrLn
          part2 g >>= putStrLn

