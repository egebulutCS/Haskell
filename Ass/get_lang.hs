import System.IO  
import Data.Char
import System.Environment
  
main = do  
   [filename] <- getArgs
   contents <- readFile filename
   print (decide contents)

eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21,1.01,3.35,4.21,13.19,1.07,1.08,1.22,5.49,0.30,0.13,3.00,5.07,5.02,10.22,3.01,1.10,6.73,7.35,5.07,4.46,1.72,0.05,0.28,0.04,0.45]

getLower :: String -> String
getLower s = [toLower c | c <- s, c /= ' ']

percent :: Int -> Int -> Float
percent x y = (a * 100) / b
    where a = fromIntegral x :: Float
          b = fromIntegral y :: Float

count_letter :: Char -> String -> Float
count_letter l s = percent (length [x | x <- s, l == x]) (length s)

freq_letter_pc :: String -> [Float]
freq_letter_pc s = [count_letter c (getLower s) | c <- ['a'..'z'], c `elem` (getLower s)]

equality :: [Float] -> [Float] -> Float
equality [x] [y] = x - y
equality (x:xs) (y:ys) = (x - y) + (equality xs ys)

decide :: String -> String
decide s = if (equality eng_freq (freq_letter_pc s)) > (equality pt_freq (freq_letter_pc s)) then "The text is in English" else "The text is in Portuguese"