import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Data.String.Parser

import System.File

wordParser : Parser String
wordParser = do chars <- some letter
                pure $ pack chars
outputParser : Parser (List String)
outputParser = do skip $ wordParser `sepBy` spaces
                  spaces
                  token "|"
                  wordParser `sepBy` spaces

parseOutput : String -> Maybe (List String)
parseOutput s = case parse outputParser s of
                     Left _ => Nothing
                     Right (l, _) => Just l

parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

isUnique : String -> Bool
isUnique s = let l = length s in
                 l == 2 || l == 3 || l == 4 || l == 7

run : String -> IO String
run s = do let l = parseLines parseOutput s
           let r = count isUnique $ join l
           pure $ show r

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s >>= putStrLn

