import Data.Either
import Data.List
import Data.Maybe
import Data.String

import System.File

data Command = Forward Int | Up Int | Down Int

parseCommand : String -> Maybe Command
parseCommand s = do (c, i) <- pure $ break (== ' ') s
                    i <- parseInteger {a=Int} i
                    case c of
                         "forward" => Just $ Forward $ i
                         "up" => Just $ Up $ i
                         "down" => Just $ Down $ i
                         _ => Nothing

parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

doCommand : (Int, Int, Int) -> Command -> (Int, Int, Int)
doCommand (p, d, a) (Forward x) = (p + x, d + a * x, a)
doCommand (p, d, a) (Up x) = (p, d, a - x)
doCommand (p, d, a) (Down x) = (p, d, a + x)

run : String -> IO ()
run s = do let (p, d, _) = foldl doCommand (0, 0, 0) $ parseLines parseCommand s
           putStrLn $ show $ p * d

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s

