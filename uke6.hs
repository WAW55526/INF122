-- A
remg :: [a] -> (a -> Bool) -> [a]
remg [] _ = []
remg (x:xs) charfilter =
    if charfilter x
        then xs
        else x : remg xs charfilter


-- 7.9 (i boken er 7.9 heile oppgaveseksjonen til kap 7, så eg tolker 7.9 som oppgave 9 i 7.9, alstå "altMap")
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap func1 func2 (x:xs) = func1 x : altMap2 func1 func2 xs

altMap2 :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap2 _ _ [] = []
altMap2 func1 func2 (x:xs) =  func2 x : altMap func1 func2 xs