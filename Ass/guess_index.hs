import System.IO  
import Data.Char
import Data.List
import System.Environment
  
main = do  
   [filename,dictionary] <- getArgs
   contents <- readFile filename
   dict <- readFile dictionary
   writeFile (filename++".dpt") (decide contents dict)
   print (decide contents dict)

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
 | isLower c = int2let ((let2int c + n) `mod` 26)
 | otherwise = c

getLower :: String -> String
getLower s = [toLower c | c <- s, c /= ' ']

translate :: Int -> String -> String
translate n s = [shift n (toLower c) | c <- s]

check_word :: String -> String -> Int
check_word word dict = if word `isInfixOf` dict then 1 else 0

word_count :: String -> String -> String -> Int -> Int
word_count [x] word dict acc = acc + (check_word (word ++ [x]) dict)
word_count (x:xs) word dict acc = if (x /= ' ') then word_count xs (word ++ [x]) dict acc else word_count xs "" dict (acc + (check_word word dict))

find_word_count :: String -> String -> [Int]
find_word_count s dict = [(word_count (translate n s) "" dict 0) | n <- [0..25]]

get_index :: Int -> [Int] -> Int
get_index t (x:xs) | x /= t = 1 + get_index t xs
                   | x == t = 0

get_n :: String -> String -> [Int] -> Int
get_n s dict [x,y] = if x > y then get_index x (find_word_count s dict) else get_index y (find_word_count s dict)
get_n s dict (x:y:xs) = if x > y then get_n s dict (x:xs) else get_n s dict (y:xs)

decide :: String -> String -> String
decide s dict = translate (get_n s dict (find_word_count s dict)) s