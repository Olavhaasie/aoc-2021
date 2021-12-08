import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Data.String.Parser

import System.File

wordParser : Parser String
wordParser = do chars <- some letter
                pure $ pack chars
outputParser : Parser (List String, List String)
outputParser = do i <- wordParser `sepBy` spaces
                  spaces
                  token "|"
                  o <- wordParser `sepBy` spaces
                  pure (i, o)

parseOutput : String -> Maybe (List String, List String)
parseOutput s = case parse outputParser s of
                     Left _ => Nothing
                     Right (l, _) => Just l

parseLines : (String -> Maybe a) -> String -> List a
parseLines f s = catMaybes $ f <$> lines s

lenEq : Nat -> String -> Bool
lenEq n s = length s == n

sortString : String -> String
sortString s = pack $ sort $ unpack s

decodeall : List String -> Maybe (List (String, String))
decodeall l = do one <- find (lenEq 2) l
                 four <- find (lenEq 4) l
                 seven <- find (lenEq 3) l
                 eight <- find (lenEq 7) l
                 let zeroorsixornine = filter (lenEq 6) l
                 nine <- find (\x => (==) 4 $ length $ intersect (unpack x) (unpack four)) zeroorsixornine
                 let zeroorsix = delete nine zeroorsixornine
                 six <- find (\x => (==) 1 $ length $ intersect (unpack x) (unpack one)) zeroorsix
                 zero <- head' $ delete six $ zeroorsix
                 let twothreeorfive = filter (lenEq 5) l
                 three <- find (\x => (==) 2 $ length $ intersect (unpack x) (unpack one)) twothreeorfive
                 let twoorfive = delete three twothreeorfive
                 two <- find (\x => (==) 2 $ length $ ((\\) `on` unpack) six x) twoorfive
                 five <- head' $ delete three $ delete two $ twoorfive
                 pure $ [
                   (sortString $ zero, "0"),
                   (sortString $ one, "1"),
                   (sortString $ two, "2"),
                   (sortString $ three, "3"),
                   (sortString $ four, "4"),
                   (sortString $ five, "5"),
                   (sortString $ six, "6"),
                   (sortString $ seven, "7"),
                   (sortString $ eight, "8"),
                   (sortString $ nine, "9")
                   ]

decode : (List String, List String) -> Maybe Nat
decode (i, o) = do m <- decodeall i
                   parsePositive $ fastAppend $ catMaybes $ ((flip lookup) m . sortString <$> o)

run : String -> IO String
run s = do let l = parseLines parseOutput s
           let r = sum $ catMaybes $ decode <$> l
           pure $ show r

main : IO ()
main = do Right s <- readFile "input.txt"
            | Left err => putStrLn $ show err
          run s >>= putStrLn

