-- G
addobb :: [Int] -> [Int]
addobb [] = []
addobb k = k ++ [x*2 | x <- k]

-- H
pali :: (Eq a) => [a] -> Bool
pali [] = True
pali x = x == reverse x
