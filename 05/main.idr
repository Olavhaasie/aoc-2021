import Data.Either
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.String.Parser

import System.File

Pipe : Type
Pipe = (Nat, Nat, Nat, Nat)

pipeParser : Parser Pipe
pipeParser = do x1 <- natural
                token ","
                y1 <- natural
                spaces1
                token "->"
                x2 <- natural
                token ","
                y2 <- natural
                pure (x1, y1, x2, y2)

parsePipe : String -> Maybe Pipe
parsePipe s = case parse pipeParser s of
                   Left _ => Nothing
                   Right (p, _) => Just p

parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

pipeToCoords : Pipe -> List (Nat, Nat)
pipeToCoords (x1, y1, x2, y2) = case (x1 == x2, y1 == y2) of
                                     (True, True) => (x1, y1) :: []
                                     (True, False) => (x1, y1) :: pipeToCoords (x1, if y1 < y2 then y1 + 1 else minus y1 1, x2, y2)
                                     (False, True) => (x1, y1) :: pipeToCoords (if x1 < x2 then x1 + 1 else minus x1 1, y1, x2, y2)
                                     (False, False) => []

run : String -> IO ()
run s = do let g = group $ sort $ join $ pipeToCoords <$> (filter (\(x1,y1,x2,y2) => x1 == x2 || y1 == y2) $ parseLines parsePipe s)
           let c = (length . forget) <$> g
           let r = length $ filter (> 1) c
           putStrLn $ show r

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

