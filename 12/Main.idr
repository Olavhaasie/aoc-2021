import Data.List
import Data.List1
import Data.SortedMap as M
import Data.String.Parser

import System.File


data Cave = Large String | Small String

Eq Cave where
  (Large x) == (Large y) = x == y
  (Small x) == (Small y) = x == y
  _ == _ = False

isSmall : Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

getName : Cave -> String
getName (Large x) = x
getName (Small x) = x

Show Cave where
  show = getName

Ord Cave where
  compare c1 c2 = compare (getName c1) (getName c2)


data Edge : Type where
  CaveEdge : Cave -> Cave -> Edge
  StartFrom : Cave -> Edge
  ToEnd : Cave -> Edge

Show Edge where
  show (CaveEdge x y) = getName x ++ "-" ++ getName y
  show (StartFrom x) = "start-" ++ getName x
  show (ToEnd x) = "end-" ++ getName x

EdgeMap : Type
EdgeMap = SortedMap Cave (List Cave)

record Caves where
  constructor MkCaves
  start : List Cave
  edges : EdgeMap
  end : List Cave

emptyCaves : Caves
emptyCaves = MkCaves [] empty []

lookupCave : Cave -> Caves -> List Cave
lookupCave c cs = maybe [] id $ M.lookup c $ cs.edges

isStart : Cave -> Caves -> Bool
isStart c caves = elem c (caves.start)

caveParser : Parser Cave
caveParser = (Small <$> takeWhile1 isLower) <|> (Large <$> takeWhile1 isUpper)

caveEdgeParser : Parser Edge
caveEdgeParser = do c1 <- caveParser
                    token "-"
                    c2 <- caveParser
                    pure $ CaveEdge c1 c2

startParser : Parser Edge
startParser =
  (token "start" *> token "-" *> StartFrom <$> caveParser) <|>
  (StartFrom <$> caveParser <* token "-" <* token "start")

endParser : Parser Edge
endParser =
  (token "end" *> token "-" *> ToEnd <$> caveParser) <|>
  (ToEnd <$> caveParser <* token "-" <* token "end")

parser : Parser (List Edge)
parser = some ((startParser <|> endParser <|> caveEdgeParser) <* spaces)

insertCave : Cave -> Cave -> EdgeMap -> EdgeMap
insertCave from to m = case lookup from m of
                            Just l  => insert from (to :: l) m
                            Nothing => insert from [to] m

toCaveMap : List Edge -> Caves -> Caves
toCaveMap [] = id
toCaveMap ((CaveEdge x y) :: xs) =
  toCaveMap xs . { edges $= insertCave y x . insertCave x y }
toCaveMap ((StartFrom x) :: xs) = toCaveMap xs . { start $= (::) x }
toCaveMap ((ToEnd x) :: xs) = toCaveMap xs . { end $= (::) x }


hasDuplicate : Eq a => List a -> Bool
hasDuplicate [] = False
hasDuplicate (x :: xs) = hasDuplicate' x xs
  where
    hasDuplicate' : a -> List a -> Bool
    hasDuplicate' y [] = False
    hasDuplicate' y (x :: xs) = y == x || hasDuplicate' x xs

mayVisit : List Cave -> Cave -> Bool
mayVisit l (Large _) = True
mayVisit l c@(Small _) = not (elem c l) || not (hasDuplicate $ sort $ filter isSmall $ l) -- this is second part

mutual
  path : Caves -> List1 Cave -> List (List1 Cave)
  path caves p@(c ::: cs) = let ps = paths' caves $ (\x => cons x p) <$> (filter (mayVisit (c :: cs)) $ lookupCave c caves) in
                                if isStart c caves
                                   then p :: ps
                                   else ps

  paths' : Caves -> List (List1 Cave) -> List (List1 Cave)
  paths' caves ps = join $ path caves <$> ps

paths : Caves -> List (List1 Cave)
paths c = paths' c (singleton <$> c.end)

run : String -> IO ()
run s = do Right (edges, _) <- pure $ parse parser s
             | Left err => putStrLn $ show err
           let caves = toCaveMap edges emptyCaves
           putStrLn $ show $ length $ paths caves

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

