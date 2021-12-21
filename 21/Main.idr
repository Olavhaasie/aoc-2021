import Data.Nat
import Control.Monad.State
import Control.Monad.State.State
import Control.Monad.State.Interface

record Player where
  constructor MkPlayer
  score : Nat
  pos : Nat

Show Player where
  show (MkPlayer score pos) = "(s=" ++ show score ++ ",p=" ++ show pos ++ ")"

record Game where
  constructor MkGame
  player1, player2 : Player
  turn : Nat
  rolls : Nat

Show Game where
  show (MkGame player1 player2 turn rolls) = show player1 ++ "\n" ++ show player2 ++ "\n" ++ show turn ++ "\n" ++ show rolls

fromStart : Nat -> Nat -> Game
fromStart p1 p2 = MkGame (MkPlayer 0 p1) (MkPlayer 0 p2) 0 0

score : Game -> Nat
score (MkGame p1 p2 _ rolls) = if p1.score > p2.score
                                  then p2.score * rolls
                                  else p1.score * rolls

rollDet : State Game Nat
rollDet = do rolls <- gets (.rolls)
             modify { rolls $= S }
             pure $ 1 + (modNatNZ rolls 100 SIsNonZero)

movePlayer : Nat -> Nat -> Nat
movePlayer pos move = (modNatNZ (pos + move) 10 SIsNonZero)

playDet : State Game ()
playDet = do turn <- gets (.turn)
             if (modNatNZ turn 2 SIsNonZero) == 0
                then do p1 <- gets (.player1.pos)
                        r1 <- rollDet
                        r2 <- rollDet
                        r3 <- rollDet
                        let pos = movePlayer p1 (r1 + r2 + r3)
                        modify { player1.score $= (+) (S pos) , player1.pos := pos }
                else do p2 <- gets (.player2.pos)
                        r1 <- rollDet
                        r2 <- rollDet
                        r3 <- rollDet
                        let pos = movePlayer p2 (r1 + r2 + r3)
                        modify { player2.score $= (+) (S pos) , player2.pos := pos }
             s1 <- gets (.player1.score)
             s2 <- gets (.player2.score)
             if s1 >= 1000 || s2 >= 1000
                then pure ()
                else do modify { turn $= S }
                        playDet

part1 : Game -> IO String
part1 a = do let g = execState a playDet
             pure $ show $ score g


playDirac : State Game (Nat, Nat)
playDirac = ?p

part2 : Game -> IO String
part2 a = do let g = execState a playDirac
             pure $ show $ score g


main : IO ()
main = do let g = fromStart 2 9
          part1 g >>= putStrLn
          part2 g >>= putStrLn

