{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Monad
import Data.Char (isDigit, digitToInt)
import Prelude (Char, Int, Bool, String, Show, IO,
                (+), (-), (*), (.),
                even, odd, fst, snd,
                otherwise, replicate, getChar)

--- Functor examples

inc :: Functor f => f Int -> f Int
inc = fmap (+1)

--- Applicative examples

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys

getChars :: Int -> IO String
getChars 0 = pure []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

--- Effectful programming

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

main :: IO String
main = getChars 3

--- Relabelling trees

tree :: Tree Char
tree  = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _)   n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                         (l',n')  = rlabel l n
                         (r',n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- mlabel :: Tree a -> ST (Tree Int)
-- mlabel (Leaf _)   = do n <- fresh
--                        return (Leaf n)
-- mlabel (Node l r) = do l' <- mlabel l
--                        r' <- mlabel r
--                        return (Node l' r')


-- I had to desugar in order to get this function to compile
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   =  fresh >>= \n -> return (Leaf n)
mlabel (Node l r) =  mlabel l >>= \l' ->
                     mlabel r >>= \r' ->
                     return (Node l' r')

--- Generic Functions

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing


----------------------------------------------------------------------
--- Exercises
----------------------------------------------------------------------

--- 1
data BinTree a = BLeaf | BNode (BinTree a) a (BinTree a)
  deriving Show

instance Functor BinTree where
  -- fmap :: (a -> b) -> f a -> f b      (This is the definition.)
  -- fmap :: (a -> b) -> BinTree a -> BinTree b
  fmap g (BLeaf)       = BLeaf
  fmap g (BNode l n r) = BNode (fmap g l) (g n) (fmap g r)

bintree :: BinTree Int
bintree = BNode (BNode (BLeaf) 1 (BNode (BLeaf) 2 (BLeaf))) 3
                (BNode (BLeaf) 4 (BNode (BNode (BLeaf) 6 (BLeaf)) 5 (BLeaf)))

out1 :: BinTree Bool
out1 = fmap even bintree

--- 2
instance Functor ((->) c) where
  -- fmap :: (a -> b) -> f a -> f b     (This is the definition.)
  -- fmap :: (a -> b) -> ((->) c a) -> ((->) c b)
  -- fmap :: (a -> b) -> (c -> a) -> (c -> b)
  fmap = (.)

out2 :: Bool
out2 = (fmap even (+1)) 3


--- 3
instance Applicative ((->) c) where
  -- pure :: a -> f a        (This is the definition.)
  -- pure :: a -> ((->) c) a
  -- pure :: a -> (c -> a)
  pure x = (\y -> x)  -- same as "const"

  -- (<*>) :: f (a -> b) -> f a -> f b      (This is the definition.)
  -- (<*>) :: ((->) c) (a -> b) -> ((->) c) a -> ((->) c) b
  -- (<*>) :: (c -> a -> b) -> (c -> a) -> (c -> b)
  g <*> h = \x -> g x (h x)

--- 4
newtype ZipList a = Z [a]
  deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z [x]

  -- (<*>) :: ZipList (a-> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | g <- gs, x <- xs]
