-- C
s :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
s = \ f g x -> f x (g x)

k :: p1 -> p2 -> p1
k = \ x y -> x

-- Me vurderer resultatet av (s k k):
-- s = \ k k x -> k x (k x)
-- s k k = \ x -> k x (k x)
-- Funksjonen k returnerer berre den første variabelen og ignorerer den andre så "k x (k x) = x"
-- s k k = \ x -> x


-- F
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] y = []
rem1 (x:xs) y
    | x == y     = xs
    | otherwise  = x : rem1 xs y