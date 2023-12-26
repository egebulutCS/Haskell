double x = x + x
quadruple x = double (double x)

-- let numb = 5
-- double numb
-- take (double 2) [1..6]

square x = x * x
sumOfDoubles x y = (square x) + (square y)

listAverage l = div (sum l) (length l)

palindrome l = l == reverse l

take4th x = head (drop 3 x)

-- [x*2 | x <- [1..10], x*2 >= 12]

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- [ x | x <- [1..50], mod x 6 == 0]

psychoLength xs = sum [1 | _ <- xs]

getCapitalLetters st = [c | c <- st, c `elem` ['A'..'Z']]

factors :: Int -> [Int]
factors n = [x | x <- [1..n] , mod n x == 0]

isPrime :: Int -> Bool
isPrime n = (length (factors n)) == 2
-- isPrime n = factors n == [1,n]

allPrime :: Int -> Int -> [Int]
allPrime lower upper = [x | x <- [lower..upper], isPrime x]

concat xss = [x | xs <- xss, x <- xs]

triad lower upper = [[x,y,z] | x <- [lower..upper], y <- [lower..upper], z <- [lower..upper], (square x) + (square y) == (square z)]

getIndex :: Eq a => a -> [a] -> Int
getIndex x xs = head [n | n <- [0..((length xs)-1)], (xs !! n) == x]

-- getIndexZip x xs = zip [n | n <- [0..((length xs)-1)]] [x | e <- xs, e == x]

scalarProduct xs ys = sum [(head(drop x xs)) * (head(drop x ys)) | x <- [0..((length xs)-1)]]
-- (head(drop x xs)) == xs!!x

type Person = String
type Book = String
type Loan = [(Person,Book)]
loans :: Loan
loans = [("Alice","TinTin"),("Anna","War and Peace"), ("Alice","Asterix"),("Rose","Macbeth")]

book_by_person :: Person -> [Book]
book_by_person person = [b | (p, b) <- loans, person == p]

-- myCompare :: (Ord a) => a -> a -> Ordering
-- myCompare a b | a > b = GT
--			   | a == b = EQ
--			   | otherwise = LT

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
-- factorial (n+1) = (n+1) * factorial n

-- pred :: Int -> Int
-- pred (n+1) = n

-- product :: [Int] -> Int
-- product [] = 1
-- product (n:ns) = n * product ns

-- length :: [a] -> Int
-- length [] = 0
-- length (_:xs) = 1 + length xs

-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]

-- zip :: [a] -> [b] -> [(a,b)]
-- zip [] _ = []
-- zip _ [] = []
-- zip (x:xs) (y:ys) = (x,y) : zip xs ys

count_one :: [Int] -> Int
count_one list = sum [1 | n <- list, n == 1]

sum10 :: Int -> [(Int,Int,Int,Int)]
sum10 n = [(x,y,z,w) | x <- [0..n], y <- [0..n], z <- [0..n], w <- [0..n], x+y+z+w == 10]

pos_one :: [Int] -> Int
pos_one (x:xs) | x == 0 = 1 + pos_one xs
               | x == 1 = 1
-- pos_one xs = head [pos | (pos,x) <- zip [0..] xs, x == 1]

league :: [String] -> [(String,String)]
league list = [(home,away) | home <- list, away <- list, home /= away]

power :: Int -> Int -> Int
power 0 y = 1
power x y = y * power (x-1) y

first_n_odds :: Int -> [Int]
first_n_odds n = [x | x <- [0..n*2], odd x]
-- first_n_odds n = [x | x <- [0..n*2], x `mod` 2 /= 0]

count_letter :: Char -> String -> Int
count_letter l s = length [x | x <- s, l == x]

c_vowel :: String -> [(Char,Int)]
c_vowel s = [(vow, count_letter vow s) | vow <- ['a','e','i','o','u']]

ascending :: [Int] -> Bool
ascending [x,xs] | x > xs = False
                 | otherwise = True
ascending (y:x:xs) | y > x = False
                   | otherwise = ascending (x:xs)

-- !! operator gives the element at index


-- quick sort
-- f [] = []
-- f (x:xs) = f ys ++ [x] ++ f zs
           -- where
             -- ys = [a | a <- xs, a <= x]
             -- zs = [b | b <- xs, b > x]

replicateN :: Int -> a -> [a]
replicateN n a = [a | x <- [1..n]]
-- replicateN 0 _ = []
-- replicateN n x = x : replicateN (n-1) x

getNth :: [a] -> Int -> a
getNth (x:xs) 0 = x
getNth [] _ = error "Index Too Large"
getNth (x:xs) n = getNth xs (n-1)

elemOfList :: Eq a => a -> [a] -> Bool
elemOfList y [] = False
elemOfList y (x:xs) | x == y = True
                    | otherwise = elemOfList y xs

sortMergeList :: (Ord a) => [a] -> [a] -> [a]
sortMergeList xs ys = mergeSort (qsort xs) (qsort ys)

mergeSort :: (Ord a) => [a] -> [a] -> [a]
-- mergeSort :: [Int] -> [Int] -> [Int]
mergeSort [] [] = []
mergeSort [] xs = xs
mergeSort xs [] = xs
mergeSort (x:xs) (y:ys) | x > y = [y] ++ [x] ++ mergeSort xs ys
                        | otherwise = [x] ++ [y] ++ mergeSort xs ys

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                 where
                   smaller = [a | a <- xs, a <= x]
                   larger = [b | b <- xs, b > x]


-- Higher order functions

max_f :: (Enum a, Ord a, Floating a) => (a -> a) -> a -> a -> a -> a
max_f h a b gap = maximum [h x | x <- [a,a+gap..b]]

parabole :: Float -> Float
parabole x = -x*x +3*x +5

-- define boolean data type
-- data Bool = False | True

-- data Answer = Yes | No | Unknown

-- answers :: [Answer]
-- answers :: [Yes,No,Uknown]

-- flip :: Answer -> Answer
-- flip Yes = No
-- flip No = Yes
-- flip Unknown = Unknown

-- recursive data type
-- data Nat = Zero | Succ Nat

data Tree = Leaf Int | Node Tree Int Tree

-- Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m==n
occurs m (Node l n r) = m==n
                        || occurs m l
                        || occurs m r

occursSorted :: Int -> Tree -> Bool
occursSorted m (Leaf n) = m==n
occursSorted m (Node l n r) | m==n = True
                            | m<n = occursSorted m l
                            | m>n = occursSorted m r

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

completeTree :: Tree -> Tree -> Bool
completeTree (Leaf n) (Leaf m) = n == m
completeTree (Node l n r) (Node l1 n1 r1) = n == n1 && completeTree l r && completeTree l1 r1

completeTreeCheck :: Tree -> Bool
completeTreeCheck (Node l n r) = completeTree l r

