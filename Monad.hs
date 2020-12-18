{-# LANGUAGE NoImplicitPrelude #-}

module Monad
  ( Functor (fmap), (<$>),
    Applicative (pure, (<*>)),
    sequenceA,
    Monad (return, (>>=)),
    app,
    ST (S),
    State,
    Maybe (Nothing, Just),
    Tree (Leaf, Node),
    mapM,
    filterM,
    join
  )
where

import Prelude (Int, Bool, IO, Show, (++), return)

----------------------------------------------------------------------
--- 12.1 Functors
----------------------------------------------------------------------
data Maybe a = Nothing | Just a
  deriving Show

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  -- fmap :: (a-> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Functor IO where
  -- fmap :: (a -> b) -> IO a -> IO b
  fmap g mx = do { x <- mx; Prelude.return (g x) }

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  -- fmap = map
  fmap _ [] = []
  fmap g (x:xs) = [g x] ++ fmap g xs

----------------------------------------------------------------------
--- 12.2 Applicatives
----------------------------------------------------------------------

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

--- Examples

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing  <*> _  = Nothing
  (Just g) <*> mx = fmap g mx

instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x]

  -- (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [g x | g <- gs, x <- xs]

instance Applicative IO where
  -- pure :: a -> IO a
  pure = Prelude.return

  -- (<*>) :: IO (a -> b) -> IO a -> IO b
  mg <*> mx = do { g <- mg; x <- mx; Prelude.return (g x) }

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

(<$>) :: Functor f => (a -> b) -> f a -> f b
g <$> x = fmap g x

----------------------------------------------------------------------
--- 12.3 Monads
----------------------------------------------------------------------

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  return = pure

--- Examples

instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing  >>= _ = Nothing
  (Just x) >>= f = f x

instance Monad [] where
  -- (>>=) :: [a] -> (a -> Maybe b) -> Maybe b
  xs >>= f = [y | x <- xs, y <- f x]

--- The state monad

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
                     let (f,s')  = app stf s
                         (x,s'') = app stx s'
                     in
                       (f x, s''))

instance Monad ST where
  -- return :: a -> m a
  return = pure

  -- (>>=) :: ST a -> (a ->ST b) -> ST b
  st >>= f = S (\s -> let (x,s') = app st s
                      in
                        app (f x) s')

--- Generic functions

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = Monad.return []
mapM f (x:xs) = f x >>= \y ->
                mapM f xs >>= \ys ->
                Monad.return (y:ys)

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ []     = Monad.return []
filterM p (x:xs) = (p x) >>= \b ->
                   filterM p xs >>= \ys ->
                   Monad.return (if b then x:ys else ys)

join :: Monad m => m (m a) -> m a
join mmx = mmx >>= \mx ->
           mx >>= \x ->
           Monad.return x
