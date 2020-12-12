module Main where

import Data.Char (digitToInt, isDigit)
import System.IO (hSetEcho, stdin)

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)


main :: IO ()
main = hangman


----------------------------------------------------------------------
--- Hangman
----------------------------------------------------------------------

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '*'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!"
               else
                  do putStrLn (match word guess)
                     play word

match :: String -> String -> String
match xs ys =
  [if elem x ys then x else '-' | x <- xs]

----------------------------------------------------------------------
--- Game of Nim
----------------------------------------------------------------------

next :: Int -> Int
next 1 = 2
next 2 = 1
next x = x

type NimBoard = [Int]

initial :: NimBoard
initial = [5,4,3,2,1]

finished :: NimBoard -> Bool
finished = all (== 0)

valid :: NimBoard -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: NimBoard -> Int -> Int -> NimBoard
move board row num = [update r n | (r,n) <- zip [1..] board]
 where
   update :: Int -> Int -> Int
   update r n = if r == row then (n-num) else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: NimBoard -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

newline :: IO ()
newline = putChar '\n'

----------------------------------------------------------------------
--- Note that the getDigit functions works when run in ghci, but does
--- not work when the compiled program is run from the command line.
----------------------------------------------------------------------

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if (isDigit x) then
                       return (digitToInt x)
                     else
                       do putStrLn "ERROR: Invalid digit"
                          getDigit prompt

turn :: NimBoard -> Int -> IO ()
turn board player =
  do newline
     putBoard board
     if finished board then
       do newline
          putStr "Player "
          putStr (show (next player))
          putStrLn " wins!"
     else
       do newline
          putStr "Player "
          putStrLn (show player)
          row <- (getDigit "Enter a row number: ")
          num <- (getDigit "Stars to remove:  ")
          if valid board row num then
            turn (move board row num) (next player)
          else
            do newline
               putStrLn "ERROR: Invalid move"
               turn board player

nim :: IO ()
nim = turn initial 1

----------------------------------------------------------------------
--- Conway's Game of Life
----------------------------------------------------------------------

-- Screen Utilities

--- See: https://en.wikipedia.org/wiki/ANSI_escape_code
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

--- See: https://en.wikipedia.org/wiki/ANSI_escape_code
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

-- Game

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

glider :: Board
glider = [(4,1), (2,2), (4,2), (3,3), (4,3)]

checkerboard :: Board
checkerboard = [(1,1), (1,3), (1,5), (1,7), (1,9),
                (2,2), (2,4), (2,6), (2,8), (2,10),
                (3,1), (3,3), (3,5), (3,7), (3,9)]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

-- allCells :: Int -> Int -> [Pos]
-- allCells w h = [(x,y) | x <- [1..width], y <- [1..height]]

-- showAllCells :: Board -> IO ()
-- showAllCells b = sequence_ [writeat p s | (p,s) <- allCells b width height]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1), (x+1,y-1),
                          (x-1,y  ),          (x+1,y  ),
                          (x-1,y+1), (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            -- _ <- getLine
            wait 1000000
            life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]


----------------------------------------------------------------------
--- Chapter 10 Exercises
----------------------------------------------------------------------

-- 1

-- putStr :: String -> IO ()
-- putStr []     = []
-- putStr (x:xs) = do putChar x
--                    Main.putStr xs

myPutStr :: String -> IO ()
myPutStr s = sequence_ (map putChar s)


-- 2

-- putRow :: Int -> Int -> IO ()
-- putRow row num = do putStr (show row)
--                     putStr ": "
--                     putStrLn (concat (replicate num "* "))

-- putBoard :: NimBoard -> IO ()
-- putBoard [a,b,c,d,e] = do putRow 1 a
--                           putRow 2 b
--                           putRow 3 c
--                           putRow 4 d
--                           putRow 5 e

myPutBoard :: NimBoard -> IO ()
myPutBoard b = myPutBoardAux b (length b)

myPutBoardAux :: NimBoard -> Int -> IO ()
myPutBoardAux [] _ = newline
myPutBoardAux (n:ns) r = do putRow r n
                            myPutBoardAux ns (r-1)


-- 3

myPutBoard' :: NimBoard -> IO ()
myPutBoard' b = sequence_ [putRow r (b !! (len-r)) | r <- reverse [1..len]]
  where len = length b

-- 4

adder :: IO ()
adder = do putStr "How many numbers? "
           numString <- getLine'
           let n = (read numString :: Int) in
             adderAux 0 n

adderAux :: Int -> Int -> IO ()
adderAux tot 0 = do putStr "The total is: "
                    putStrLn (show tot)
adderAux tot n = do numString <- getLine'
                    let v = (read numString :: Int) in
                      adderAux (tot+v) (n-1)

-- 5

adder' :: IO ()
adder' =
  do putStr "How many numbers? "
     numString <- getLine'
     let n = (read numString :: Int) in
       do numbers <- sequence (replicate n getLine')
          putStr "The total is: "
          -- putStrLn (show (sum (map (\x -> read x :: Int) numbers)))
          putStrLn . show . sum $ map (\x -> read x :: Int) numbers

-- The last two lines are eqivalent, but the second is nicer to read
-- because it has fewer parentheses.
