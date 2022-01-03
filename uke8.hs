import Data.List (intersperse)

-- C
trekanter :: Int -> Int -> Int -> IO ()
trekanter a b c = putStrLn (unlines (flettLister (lagTrekantListe a) (lagTrekantListe b) (lagTrekantListe c) a b c))

lagTrekantListe :: Int -> [String]
lagTrekantListe h = [concat (replicate (h-i) " ") ++ intersperse ' ' (replicate i '*') ++ concat (replicate (h-i) " ") | i <- [1 .. h]]

flettLister :: [String] -> [String] -> [String] -> Int -> Int -> Int -> [String]
flettLister [] [] [] x y z = []
flettLister xs [] [] x y z = flettLister (init xs) [] [] x y z               ++ [last xs ++ "  "       ++ replicate (2*y+1) ' ' ++ replicate (2*z-1) ' ']
flettLister [] ys [] x y z = flettLister [] (init ys) [] x y z               ++ [replicate (2*x+1) ' ' ++ last ys ++ "  "       ++ replicate (2*z-1) ' ']
flettLister [] [] zs x y z = flettLister [] [] (init zs) x y z               ++ [replicate (2*x+1) ' ' ++ replicate (2*y+1) ' ' ++ last zs]
flettLister xs ys [] x y z = flettLister (init xs) (init ys) [] x y z        ++ [last xs ++ "  "       ++ last ys ++ "  "       ++ replicate (2*z-1) ' ']
flettLister xs [] zs x y z = flettLister (init xs) [] (init zs) x y z        ++ [last xs ++ "  "       ++ replicate (2*y+1) ' ' ++ last zs]
flettLister [] ys zs x y z = flettLister [] (init ys) (init zs) x y z        ++ [replicate (2*x+1) ' ' ++ last ys ++ "  "       ++ last zs]
flettLister xs ys zs x y z = flettLister (init xs) (init ys) (init zs) x y z ++ [last xs ++ "  "       ++ last ys ++ "  "       ++ last zs]


--D
data FileOrFolder = File Int | Folder [ FileOrFolder ]

prettyPrint :: FileOrFolder -> IO ()
prettyPrint (File x) = putStrLn ("-File " ++ show x)
prettyPrint (Folder x) =
    if null x
        then putStrLn ("-Folder " ++ show (length x))
        else putStrLn ("-Folder " ++ show (length x) ++ goTroughFolder x 2)

goTroughFolder :: [FileOrFolder] -> Int -> String
goTroughFolder xs i = foldr (\ x -> (++) (prettyPrint' x i)) [] xs

prettyPrint' :: FileOrFolder -> Int -> String
prettyPrint' (File x) i = "\n" ++ replicate i ' ' ++ "-File " ++ show x
prettyPrint' (Folder x) i =
    if null x
        then "\n" ++ replicate i ' ' ++ "-Folder " ++ show (length x)
        else "\n" ++ replicate i ' ' ++ "-Folder " ++ show (length x) ++ goTroughFolder x (i+2)