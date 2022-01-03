-- D
foo1 :: a -> b -> (a, b)
foo1 x y = (x, y)

foo2 :: a -> b -> (a, b)
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a, b)
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a, b)
foo4 = \x -> \y -> (x, y)

foo5 :: b -> a -> (a, b)
foo5 = \x -> \y -> (y,x)

foo6 :: a -> b -> (a, b)
foo6 = \y -> \x -> (y,x)

-- Alle funksjonene bortsett fra foo5 er ekvivalente


-- E
f1 :: a -> (a,a)
f1 x = (x,x)

f2 :: (a,b) -> a
f2 (x,y) = x

f3 :: (a,b) -> b
f3 (x,y) = y

f4 :: a -> b -> a
f4 x y = x

f5 :: a -> b -> b
f5 x y = y


-- F
f :: Int -> Int -> Int
f x y = x*y

g :: (Int, Int) -> Int
g (x,y) = x*y
