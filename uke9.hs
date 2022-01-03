import Data.Char

-- A
gRep :: (t->Bool)->t->[t]->[t]
gRep pr y [] = []
gRep pr y (x:xs) =
    if pr x
        then y : gRep pr y xs
        else x : gRep pr y xs

gRep' :: (t->Bool)->t->[t]->[t]
gRep' pr y [] = []
gRep' pr y list = map (f pr y) list

f :: (t->Bool)->t->t->t
f pr y x = if pr x then y else x


-- D
type Board = [Int]

next :: Int -> Int
next 1 = 2
next 2 = 1

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard = putBoard' 1

putBoard' :: Int -> Board -> IO ()
putBoard' r [] = return ()
putBoard' r (n:ns) = do putRow r n
                        putBoard' (r+1) ns

getDigit :: String -> IO Int
getDigit prompt = do
    putStr prompt
    s <- getLine
    newline
    if not (null s) && all isDigit s then return (read s)
    else do
        putStrLn "ERROR: Invalid row"
        getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> [Board] -> Int -> IO ()
play board oldBoards player = do
    newline
    putBoard board
    if finished board then do
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do
        newline
        putStr "Player "
        putStrLn (show player)
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove : "
        if row == 0 && null oldBoards then do
                                            print "Can't go back further, try again"
                                            play initial [] player 
        else if row == 0 then play (last oldBoards) (init oldBoards) (next player)
        else if valid board row num then play (move board row num) (oldBoards ++ [board]) (next player)
            else do
                newline
                putStrLn "ERROR: Invalid move"
                play board oldBoards player

nim :: IO ()
nim = play initial [] 1


