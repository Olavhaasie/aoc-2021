import Data.List
import Data.Vect
import Data.String.Parser

import System.File

Vec3 : Type
Vec3 = Vect 3 Integer

minusV : Vec3 -> Vec3 -> Vec3
minusV [x1,y1,z1] [x2,y2,z2] = [x1-x2,y1-y2,z1-z2]

maxV : Vec3 -> Vec3 -> Vec3
maxV [x1,y1,z1] [x2,y2,z2] = [max x1 x2, max y1 y2, max z1 z2]

minV : Vec3 -> Vec3 -> Vec3
minV [x1,y1,z1] [x2,y2,z2] = [min x1 x2, min y1 y2, min z1 z2]

Cube : Type
Cube = (Vec3, Vec3)

isCorrect : Cube -> Maybe Cube
isCorrect c@([x1,y1,z1], [x2,y2,z2]) =
  if x1 < x2 && y1 < y2 && z1 < z2
     then Just c
     else Nothing
intersect : Cube -> Cube -> Maybe Cube
intersect (min1, max1) (min2, max2) = isCorrect (maxV min1 min2, minV max1 max2)

volume : Cube -> Integer
volume (min, max) = product $ (abs <$> minusV max min)

data Command = On Cube | Off Cube

isOn : Command -> Bool
isOn (On _) = True
isOn _ = False

invert : Command -> Command
invert (On c) = Off c
invert (Off c) = On c

getCube : Command -> Cube
getCube (On c) = c
getCube (Off c) = c

Input : Type
Input = List Command

rangeParser : Parser (Integer, Integer)
rangeParser = do skip letter
                 skip $ char '='
                 min <- integer
                 token ".."
                 max <- integer
                 pure (min, max + 1)

commandParser : Parser Command
commandParser = do cmd <- (token "on" $> On) <|> (token "off" $> Off)
                   [(xmin, xmax),(ymin,ymax),(zmin,zmax)] <- commaSep rangeParser
                     | _ => fail "no ranges"
                   pure $ cmd ([xmin,ymin,zmin],[xmax,ymax,zmax])

parser : Parser Input
parser = some (commandParser <* spaces)

Cubes : Type
Cubes = List Cube

addCube' : List Command -> Cube -> (Integer, List Command)
addCube' [] c = (0, [])
addCube' ((On c) :: cs) b = case intersect b c of
                                 Nothing => addCube' cs b
                                 (Just overlap) => let (v, os) = addCube' cs b in
                                                       (v - volume overlap, (Off overlap) :: os)
addCube' ((Off c) :: cs) b = case intersect b c of
                                  Nothing => addCube' cs b
                                  (Just overlap) => let (v, os) = addCube' cs b in
                                                       (v + volume overlap, (On overlap) :: os)

addCube : List Command -> Cube -> (Integer, List Command)
addCube cs c = case addCube' cs c of
                    (vol, []) => (vol, cs)
                    (vol, os) => (vol, os ++ cs)

doCmd : (Integer, List Command) -> Command -> (Integer, List Command)
doCmd (vol, cs) c@(On b) = let (v, cs) = addCube cs b in
                               (v + vol + volume b, c :: cs)
doCmd (vol, cs) (Off b) = mapFst ((+) vol) $ addCube cs b

cmdInBounds : Cube -> Command -> Maybe Command
cmdInBounds b (On c) = On <$> intersect b c
cmdInBounds b (Off c) = Off <$> intersect b c

part1 : Input -> IO String
part1 a = let bound : Cube = ([-50,-50,-50], [51,51,51])
              cs = catMaybes $ cmdInBounds bound <$> a in
              pure $ show $ fst $ foldl doCmd (0, []) cs

part2 : Input -> IO String
part2 a = pure $ show $ fst $ foldl doCmd (0, []) a

main : IO ()
main = do Right input <- readFile "input.txt"
            | Left err => printLn err
          Right (a, _) <- pure $ parse parser input
            | Left err => printLn err
          part1 a >>= putStrLn
          part2 a >>= putStrLn

