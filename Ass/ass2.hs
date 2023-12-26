import Data.Char

-- 1
square :: Int -> Int
square x = x * x

check_is_square :: Int -> Int -> Bool
check_is_square 1 x = (square 1) == x
check_is_square n x = if (square n) == x then True else check_is_square (n-1) x

is_square :: Int -> Bool
is_square x = check_is_square x x

-- 2
getLower :: String -> String
getLower s = [toLower c | c <- s, c /= ' ']

percent :: Int -> Int -> Float
percent x y = (a * 100) / b
    where a = fromIntegral x :: Float
          b = fromIntegral y :: Float

count_letter :: Char -> String -> Float
count_letter l s = percent (length [x | x <- s, l == x]) (length s)

freq_letter_pc :: String -> [(Char,Float)]
freq_letter_pc s = [(c, count_letter c (getLower s)) | c <- ['a'..'z'], c `elem` (getLower s)]

-- 3
cities = [(1,"Paris",7000000,1),(2,"London",8000000,2),(1,"Rome",3000000,3), (1,"Edinburgh",500000,2),(1,"Florence",50000,3), (1,"Venice",200000,3), (1,"Lyon",1000000,1),(1,"Milan",3000000,3), (1,"Madrid",6000000,4), (1,"Barcelona",5000000,4)]

countries = [(1,"UK"), (2,"France"), (3,"Italy"), (4,"Spain")]

-- a
get_city_above :: Int -> [String]
get_city_above n = [city_name | (city_id, city_name, city_population, country_id) <- cities, city_population >= n]

-- b
get_country_id :: String -> [Int]
get_country_id name = [country_id | (country_id, country_name) <- countries, country_name == name]

get_city :: String -> [String]
get_city name = [city_name | (city_id, city_name, city_population, country_id) <- cities, country_id `elem` (get_country_id name)]

-- c
get_city_num :: String -> Int
get_city_num name = length (get_city name)

num_city :: [(String,Int)]
num_city = [(country_name, get_city_num country_name) | (country_id, country_name) <- countries]

-- 4
sqr :: Float -> Float
sqr x = x * x

eucl_dist :: [Float] -> [Float] -> Float
eucl_dist [x] [y] = sqr (x + y)
eucl_dist (x:xs) (y:ys) =  sqrt ((sqr (x + y)) + (eucl_dist xs ys))

-- 5
-- can be found, compiled and run in get_lang.hs

-- 6
-- can be found, compiled and run in decrypt.hs

-- 7
-- Part1 can be found, compiled and run in make_dict.hs
-- Part2 can be found, compiled and run in guess_index.hs

-- 8
-- The question is unclear

-- 9
powerOf :: Float -> Float -> Float
powerOf x 0 = 1
powerOf x n = x * (powerOf x (n-1))

sample_series :: Float -> Float
sample_series k = 1 / (powerOf 2 (k-1))

math_series :: (Float -> Float) -> Float -> Float
math_series f 1 = f 1
math_series f n = (f n) + (math_series f (n-1))

-- 10
f :: Float -> Float
f x = 0.5 * x

find_integral :: (Float -> Float) -> Float -> Float -> Float -> Float -> Float
find_integral f x1 x2 n 0 = ((f x1) * ((x2-x1)/n))
find_integral f x1 x2 n acc = ((f (x1 + (acc * ((x2-x1)/n)))) * ((x2-x1)/n)) + (find_integral f x1 x2 n (acc-1))

integral :: (Float -> Float) -> Float -> Float -> Float -> Float
integral f x1 x2 n = find_integral f x1 x2 n (n-1)