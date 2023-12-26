import System.IO
import System.Environment

main = do
 [f] <- getArgs
 s   <- readFile f
 putStrLn (show (c_vowel s))
 putStrLn (show (total_vowels s))
 
count_letter l s = length [x | x <- s, l == x]
c_vowel s = [(vow, count_letter vow s) | vow <- ['a','e','i','o','u','A','E','I','O','U']]

total_vowels s = sum [count_letter vow s | vow <- ['a','e','i','o','u','A','E','I','O','U']]

-- count_vowels s = lenght [x | x <- s, elem x "aeiouAEIOU"]