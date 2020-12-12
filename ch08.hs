----------------------------------------
--- Chapter 8
----------------------------------------
----------------------------------------
--- 8.6 Tautology Checker
----------------------------------------

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
  deriving Show

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

-- Some propositions for testing purposes

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


-- Function for value lookup via key in Assoc

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k==k']


-- Logic functions for the tautology checker

evalProp :: Subst -> Prop -> Bool
evalProp _ (Const b)   = b
evalProp s (Var x)     = find x s
evalProp s (Not p)     = not (evalProp s p)
evalProp s (And p q)   = evalProp s p && evalProp s q
evalProp s (Imply p q) = evalProp s p <= evalProp s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

{- Original non-recursive version of Boolean table generator

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
  where
    range     = [0..(2^n)-1]
    make n bs = take n (bs ++ repeat 0)
    conv 0    = False
    conv 1    = True
-}

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where
    bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [evalProp s p | s <- substs p]

----------------------------------------
--- 8.7 Abstract Machine
----------------------------------------
{-
-- Simple Expression Type and Evaluator
data Expr = Val Int | Add Expr Expr
  deriving Show

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y

-- (2 + 3) + 4 == 9
exp1 :: Expr
exp1 = Add (Add (Val 2) (Val 3)) (Val 4)

-- (3 + (-4)) + (1 + 2) == 2
exp2 ::Expr
exp2 = Add (Add (Val 3) (Val (-4))) (Add (Val 1) (Val 2))

-- Control Stack for an Abstract Machine
type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)

value' :: Expr -> Int
value' e = eval e []
-}
----------------------------------------
--- Exercises
----------------------------------------

-- 1
data Nat = Zero | Succ Nat
  deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero     _ = Zero
mult (Succ m) n = add (mult m n) n

-- test
ans1 :: Int
ans1 = nat2int (mult (int2nat 4) (int2nat 13))

-- 2
data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x==y
occurs x (Node l y r) = x==y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- ST is short for "search tree", i.e. an ordered tree
occursST :: Ord a => a -> Tree a -> Bool
occursST x (Leaf y)                 = x==y
occursST x (Node l y r) | x==y      = True
                        | x<y       = occursST x l
                        | otherwise = occursST x r

-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering

occursST' :: Ord a => a -> Tree a -> Bool
occursST' x (Leaf y)     = case (compare x y) of
                             EQ -> True
                             LT -> False
                             GT -> False
occursST' x (Node l y r) = case (compare x y) of
                             EQ -> True
                             LT -> occursST' x l
                             GT -> occursST' x r

{- The new function, occursST' is more efficient than occursST because
 you are guaranteed that you will only have to do one comparison when
 given a Node, whereas in the old definition you may have to do two
 comparisons. In the documentation for the Prelude it states: "The
 Ordering datatype allows a single comparison to determine the precise
 ordering of two objects." But I don't understand how that is
 possible. -}

-- 3
data Tree' a  = Leaf' a | Node' (Tree' a) (Tree' a)
  deriving Show

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = nl == nr || nl == nr+1 || nl == nr-1
  where
    nl = leaves l
    nr = leaves r

t1' :: Tree' Int
t1' = Node' (Node' (Leaf' 1) (Leaf' 4))
            (Node' (Leaf' 6) (Leaf' 9))

t2' :: Tree' Int
t2' = Node' (Node' (Leaf' 1) (Node' (Leaf' 6) (Leaf' 9)))
            (Leaf' 4)

t3' :: Tree' Int
t3' = Node' (Leaf' 1)
            (Node' (Leaf' 6) (Leaf' 9))

-- 4
splitList :: [a] -> ([a],[a])
splitList xs = (take n xs, drop n xs)
  where
    n = (length xs) `div` 2

balance :: [a] -> Tree' a
balance (x:[]) = Leaf' x
balance xs     = Node' (balance left) (balance right)
  where
    (left, right) = splitList xs

-- 5
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g e = case e of
  (Val x)   -> f x
  (Add x y) -> g (folde f g x) (folde f g y)

-- 6
eval' :: Expr -> Int
eval' = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

-- 7
data MyMaybe a = MyNothing | MyJust a

instance Eq a => Eq (MyMaybe a) where
  MyJust x  == MyJust y  = x == y
  MyNothing == MyNothing = True
  _         == _         = False

data List a = Empty | Cons a (List a)

instance Eq a => Eq (List a) where
  Cons x xs == Cons y ys = x==y && xs==ys
  Empty     == Empty     = True
  _         == _         = False


-- 9
-- Simple Expression Type and Evaluator
data Expr = Val Int | Add Expr Expr | Mul Expr Expr
  deriving Show

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y
value (Mul x y) = value x * value y

-- (2 * 3) + 4 == 10
exp1 :: Expr
exp1 = Add (Mul (Val 2) (Val 3)) (Val 4)

-- (3 * (-4)) * (1 + 2) == -36
exp2 ::Expr
exp2 = Mul (Mul (Val 3) (Val (-4))) (Add (Val 1) (Val 2))

-- Control Stack for an Abstract Machine
type Cont = [Op]
data Op = EVAL Expr | ADD Int | MUL Int
  deriving Show

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)
eval (Mul x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)
exec (MUL n : c)  m = exec c (n*m)

value' :: Expr -> Int
value' e = eval e []
