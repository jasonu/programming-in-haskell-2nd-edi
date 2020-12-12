import Prelude(Ord, Num, Eq,
               Int, Bool(True, False),
               head, otherwise, div,
               (&&), (++), (+), (-), (*),
               (==), (<=), (>=), (>), (<))

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

zip :: [a] -> [b] -> [(a,b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <-xs, a <= x]
    larger  = [a | a <-xs, a >= x]

-- Mutual Recursion

even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n-1)

evens :: [a] ->[a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] ->[a]
odds [] = []
odds (_:xs) = evens xs

-- Exercises
-- 1
fac :: Int -> Int
fac 0 = 1
fac n | n>0 = n * fac (n-1)
      | otherwise = 1

-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3
(^) :: Int -> Int -> Int
_ ^ 0 = 1
b ^ n = b * (b^(n-1))

-- 4
euclid :: Int -> Int -> Int
euclid _ 1 = 1
euclid 1 _ = 1
euclid m n | m == n = m
           | m < n = euclid m (n-m)
           | m > n = euclid (m-n) n

-- 6
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

replicate :: Int -> a  -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) | x == y = True
              | otherwise = elem x ys

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x > y  = y : merge (x:xs) ys
-- 8
halve :: [a] -> ([a],[a])
halve [] = ([], [])
halve xs = (take mid xs, drop mid xs)
  where
    mid = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left,right) = halve xs

-- 9
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 xs     = []
take _ []     = []
take n (x:xs) = x : (take (n-1) xs)

last :: [a] -> a
last xs = head (reverse xs)
