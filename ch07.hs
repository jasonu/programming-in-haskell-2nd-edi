import Data.Char
import Data.List

----------------------------------------
--- 7.6 Binary string transmitter
----------------------------------------

type Bit = Int

bin2int :: [Bit] -> Int
-- bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--   where weights = iterate (*2) 1
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

----------------------------------------
--- 7.7 Voting algorithms
----------------------------------------

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x)  xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last .result

-- Ranked choice voting

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
               [c] -> c
               (c:cs) -> winner' (elim c bs)

----------------------------------------
--- Exercises
----------------------------------------

-- 1

-- [f x | x <- xs, p x]
myfunc :: (a -> b) -> (a -> Bool) -> [a] -> [b]
myfunc f p xs = (map f . filter p) xs

testmyfunc :: Bool
testmyfunc = myfunc head (/= "Red") votes == [head x | x <- votes, (/= "Red") x]

-- 2
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . (map p)

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . (map p)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = filter p

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = filter (not . p)

-- 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if (p x) then x:xs else xs) []

-- 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- 5
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

-- 6
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

--
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
--
chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

myiterate :: (a -> a) -> a -> [a]
myiterate f = unfold (\_ -> False) id f

-- 7 and 8
parity :: [Bit] -> Bit
parity bs | odd (sum bs) = 1
          | otherwise    = 0

consParityBit :: [Bit] -> [Bit]
consParityBit bs = (parity bs) : bs

myEncode :: String -> [Bit]
myEncode = concat . map (consParityBit . make8 . int2bin . ord)

chopn :: Int -> [Bit] -> [[Bit]]
chopn _ [] = []
chopn n bits = take n bits : chopn n (drop n bits)

chop9 :: [Bit] -> [[Bit]]
chop9 = chopn 9

checkParity :: [Bit] -> [Bit]
checkParity [] = []
checkParity (b:bs) | b == (parity bs) = bs
                   | otherwise = error "Parity Error"

myDecode :: [Bit] -> String
myDecode = map (chr . bin2int. checkParity) . (chop9)

myTransmit :: String -> String
myTransmit = myDecode . faultyChannel . myEncode

faultyChannel :: [Bit] -> [Bit]
faultyChannel [] = []
faultyChannel (b:bs) = bs

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ (x:[]) = (f x) : []
altMap f g (x:y:rest) = (f x) : (g y) : (altMap f g rest)
