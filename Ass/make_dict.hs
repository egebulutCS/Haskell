import System.IO  
import Data.Char
import Data.List
import System.Environment
  
main = do  
   [filename1,filename2,filename3] <- getArgs
   content1 <- readFile filename1
   content2 <- readFile filename2
   content3 <- readFile filename3
   writeFile ("dict.txt") (make_dict content1 "" (make_dict content2 "" (make_dict content3 "" "")))

check_word :: String -> String -> String
check_word word dict = if (word `isInfixOf` dict) then "" else " " ++ word

make_dict :: String -> String -> String -> String
make_dict [x] word dict = dict ++ (check_word word dict)
make_dict (x:xs) word dict = if (x /= ' ') then make_dict xs (word ++ [x]) dict else make_dict xs "" (dict ++ (check_word word dict))