import Prelude hiding (pi)

data Ast = V Int | P Ast Ast | M Ast Ast

-- C
ev :: Ast -> (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> a
ev (V x) v p m = v x
ev (P x y) v p m = p (ev x v p m) (ev y v p m)
ev (M x y) v p m = m (ev x v p m) (ev y v p m)


vi :: Int -> Int
vi x = x

pi :: Int -> Int -> Int
pi x y = x + y

mi :: Int -> Int -> Int
mi x y = x * y

vb :: Int -> Bool
vb = odd

pb :: Bool -> Bool -> Bool
pb x y = x || y

mb :: Bool -> Bool -> Bool
mb x y = x && y

vStr :: Int -> String
vStr = show

pStr :: String -> String -> String
pStr x y = "(" ++ x ++ " + " ++ y ++ ")"

mStr :: String -> String -> String
mStr x y = "(" ++ x ++ " * " ++ y ++ ")"


-- D
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)