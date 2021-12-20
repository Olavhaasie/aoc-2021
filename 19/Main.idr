import Data.List
import Data.List1
import Data.Vect
import Data.Vect.Elem
import Data.Vect.Extra
import Data.String.Parser

import System.File

Pos : Type
Pos = Vect 3 Integer

addVec : Pos -> Pos -> Pos
addVec [x1,y1,z1] [x2,y2,z2] = [x1+x2,y1+y2,z1+z2]

minVec : Pos -> Pos -> Pos
minVec [x1,y1,z1] [x2,y2,z2] = [x1-x2,y1-y2,z1-z2]

dist : Pos -> Pos -> Integer
dist p1 p2 = sum $ abs <$> minVec p1 p2

rotateVecZ : Vect 3 Integer -> List (Vect 3 Integer)
rotateVecZ [x,y,z] = [[x,y,z],[-y,x,z],[-x,-y,z],[y,-x,z]]

rotateVec3 : Vect 3 Integer -> List (Vect 3 Integer)
rotateVec3 [x,y,z] = join $ rotateVecZ <$> [[x,y,z],[-x,y,-z],[z,y,-x],[-z,y,x],[x,z,-y],[x,-z,y]]

Scanner : Type
Scanner = List Pos

Input : Type
Input = List1 Scanner


posParser : Parser Pos
posParser = do x <- integer
               skip $ char ','
               y <- integer
               skip $ char ','
               z <- integer
               pure [x, y, z]

some1 : Parser a -> Parser (List1 a)
some1 p = do Just s <- fromList <$> some p
               | Nothing => fail "none"
             pure s

scannerParser : Parser Scanner
scannerParser = token "---" *> token "scanner" *> lexeme natural *> token "---" *> some (posParser <* spaces)

parser : Parser Input
parser = some1 (scannerParser <* spaces)


rotateScanner : Scanner -> List Scanner
rotateScanner s = transpose $ rotateVec3 <$> s

moveRel : Scanner -> Pos -> Scanner
moveRel l p = map (addVec p) l

overlaps : Scanner -> Scanner -> Pos -> Bool
overlaps abs rels offset = (count ((flip elem) rels) $ (flip minVec) offset <$> abs) >= 12

overlap : Scanner -> Scanner -> Maybe (Scanner, Pos)
overlap abs rels = map (\f => (union abs $ moveRel rels f, f)) $ find (overlaps abs rels) $ (join $ (\a => minVec a <$> rels) <$> abs)

anyOverlap : Scanner -> List Scanner -> Maybe (Scanner, Pos)
anyOverlap abs [] = Nothing
anyOverlap abs (x :: xs) = overlap abs x <|> anyOverlap abs xs

findOverlap : Scanner -> List (List Scanner) -> Maybe (Scanner, Pos, List (List Scanner))
findOverlap abs [] = Nothing
findOverlap abs (rel :: rels) = case anyOverlap abs rel of
                                     Nothing => do (abs, offset, rels) <- findOverlap abs rels
                                                   pure (abs, offset, rel :: rels)
                                     (Just (abs, offset)) => Just (abs, offset, rels)

overlapall : Scanner -> List (List Scanner) -> IO (Maybe (Scanner, List Pos))
overlapall abs [] = pure $ Just (abs, [])
overlapall abs rels = do Just (abs, offset, rels) <- pure $ findOverlap abs rels
                           | Nothing => pure Nothing
                         putStrLn $ (show $ length rels) ++ "scanners left"
                         Just (all, offsets) <- overlapall abs rels
                           | Nothing => pure Nothing
                         pure $ Just (all, offset :: offsets)

part1and2 : Input -> IO String
part1and2 (x ::: xs) = do Just (bs, offsets) <- overlapall x (rotateScanner <$> xs)
                            | Nothing => pure "no overlap"
                          putStrLn $ show $ length bs
                          pure $ show $ foldl max 0 $ join $ (\o => dist o <$> offsets) <$> offsets

main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          Right (a, _) <- pure $ parse parser input
            | Left err => printLn err
          part1and2 a >>= putStrLn

