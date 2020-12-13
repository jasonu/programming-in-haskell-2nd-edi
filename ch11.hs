module Main where

import Data.Char
import Data.List
import System.IO
import AnsiConsole
import System.Random (randomRIO)

{-

Note: AnsiConsole is just a tiny module I made for the 'cls' and
'goto' commands used below. These commands were introduced in chapter
10.

Note: System.Random was not installed on my system, or at least
neither ghci, nor ghc could find it. I had to install it via the
following commands:

$ cabal update
$ cabal install --lib random

-}

----------------------------------------------------------------------
--- Tic-Tac-Toe
----------------------------------------------------------------------

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
  deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/=B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (==O) ps)
    xs = length (filter (==X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

--- Displaying a grid

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
 where
   beside = foldr1 (zipWith (++))
   bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

--- Making a move

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then
    [chop size (xs ++ [p] ++ ys)]
  else
    []
  where
    (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

--- Reading a number

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

--- Human vs human

tictactoe :: IO ()
tictactoe = run empty X

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise =
           do i <- getNat (prompt p)
              case move g i p of
                [] -> do putStrLn "ERROR: Invalid move"
                         run' g p
                [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

-- main :: IO ()
-- main = tictactoe

--- Game trees

data Tree a = Node a [Tree a]
  deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g     = []
  | full g    = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

--- Pruning the tree

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

--- Minimax algorithm

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
  | wins O g  = Node (g,O) []
  | wins X g  = Node (g,X) []
  | otherwise = Node (g,B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps  = [p | Node (_,p) _ <- ts']

-- bestmove :: Grid -> Player -> Grid
-- bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
--   where
--     tree = prune depth (gametree g p)
--     Node (_,best) ts = minimax tree

--- Human vs computer

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

-- main :: IO Int
-- main = do x <- randomRIO (0, 5)
--           return x

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

-- play' :: Grid -> Player -> IO ()
-- play' g p
--   | wins O g = putStrLn "Player O wins!\n"
--   | wins X g = putStrLn "Player X wins!\n"
--   | full g   = putStrLn "It's a draw!\n"
--   | p == O   = do i <- getNat (prompt p)
--                   case move g i p of
--                     [] -> do putStrLn "ERROR: Invalid move"
--                              play' g p
--                     [g'] -> play g' (next p)
--   | p == X   = do putStrLn "Player X is thinking..."
--                   (play $! (bestmove g p)) (next p)

----------------------------------------------------------------------
--- Exercises
----------------------------------------------------------------------

--- 1

gt :: Tree Grid
gt = gametree empty O

maxTreeDepth :: Tree a -> Int
maxTreeDepth (Node _ []) = 0
maxTreeDepth (Node _ ts) = 1 + (foldr max 0 (map maxTreeDepth ts))

countNodes :: Tree a -> Int
countNodes (Node _ []) = 1
countNodes (Node _ ts) = 1 + (foldr (+) 0 (map countNodes ts))

--- 2

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g',p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax tree



play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == O   = do i <- getNat (prompt p)
                  case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             play' g p
                    [g'] -> play g' (next p)
  | p == X   = do putStrLn "Player X is thinking..."
                  let gs = bestmoves g p
                  x <- randomRIO (0, (length gs) - 1)
                  play (gs !! x) (next p)
