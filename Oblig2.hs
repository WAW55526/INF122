import Data.List (intersperse)
import Data.Char (isDigit)


data Command =
    Quit |
    Move Int Int |
    NewGame Int |
    MoveBack Int |
    Invalid String

type Tower = [Int]
type Board = [Tower]



main :: IO()
main = do
    refreshStartScreen
    newline
    cmd <- getLine
    case parseCommandStart (words cmd) of
        Quit -> quit
        NewGame n -> do
                        clearScreen
                        showBoard (startBoard n)
                        newline
                        putStr "Number of moves: 0"
                        play (startBoard n) [] n 0
        Invalid message -> do
                            refreshStartScreen
                            putStr ("          " ++ message)
                            newline
                            main'

main' :: IO()
main' = do
    cmd <- getLine
    case parseCommandStart (words cmd) of
        Quit -> quit
        NewGame n -> do
                        clearScreen
                        showBoard (startBoard n)
                        newline
                        putStr "Number of moves: 0"
                        play (startBoard n) [] n 0
        Invalid message -> do
                            refreshStartScreen
                            putStr ("          " ++ message)
                            newline
                            main'

refreshStartScreen :: IO()
refreshStartScreen = do
    clearScreen
    startScreen

clearScreen :: IO()
clearScreen = do
                putStr "\ESC[2J"
                putStr "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
                putStr "\ESC[2J"

newline :: IO ()
newline = putChar '\n'

startScreen :: IO()
startScreen = do
    putStrLn "######################################################"
    putStrLn "#-----------------The Tower of Hanoi-----------------#"
    putStrLn "######################################################"
    newline
    putStr "Start a new game with: b <numOfRings>, or quit with: q"



play :: Board -> [Board] -> Int -> Int -> IO ()
play b bs s c =
       if finished b s then
          do clearScreen
             showBoard b
             newline
             putStrLn ("~~Congratulations, YOU WIN! You beat the game using " ++ show s ++ " rings in " ++ show c ++ " moves!~~")
             putStr "Press enter to continue..."
             halt <- getLine
             main
       else
          do newline
             cmd <- getLine
             case parseCommandInGame (words cmd) of
                 Quit -> quit
                 NewGame n -> do
                                clearScreen
                                showBoard (startBoard n)
                                newline
                                putStr "Number of moves: 0"
                                play (startBoard n) [] n 0
                 Move f t -> if move b f t == b
                                then do
                                    clearScreen
                                    showBoard b
                                    newline
                                    putStr ("Number of moves: " ++ show c ++ "          --Illegal move--")
                                    play b bs s c
                                else do
                                    clearScreen
                                    showBoard (move b f t)
                                    newline
                                    putStr ("Number of moves: " ++ show (c+1))
                                    play (move b f t) (bs ++ [b]) s (c+1)
                 MoveBack m -> if m < 1
                                then do
                                    clearScreen
                                    showBoard b
                                    newline
                                    putStr ("Number of moves: " ++ show c ++ "          --Can't move back less than 1 move--")
                                    play b bs s c
                                else do
                                    clearScreen
                                    showBoard (getMoveBackBoard b bs m)
                                    newline
                                    if c-m < 0
                                        then putStr "Number of moves: 0"
                                        else putStr ("Number of moves: " ++ show (c-m))
                                    moveBack b bs s c m

                 Invalid message -> do
                                    clearScreen
                                    showBoard b
                                    newline
                                    putStr ("Number of moves: " ++ show c ++ "          " ++ message)
                                    play b bs s c



parseCommandStart :: [String] -> Command
parseCommandStart ls = case ls of
   ["q"]    -> Quit
   "b":[n]  -> if all isDigit n
                then if read n < 1 || 10 < read n
                    then Invalid "--Choose number of rings between 1 and 10--"
                    else NewGame (read n)
                else Invalid "--b <numOfRings> needs a positive number between 1 and 10--"
   _ -> Invalid "--Unrecognized command--"

parseCommandInGame :: [String] -> Command
parseCommandInGame ls = case ls of
   ["q"]    -> Quit
   "b":[n]  -> if all isDigit n
                then if read n < 1 || 10 < read n
                    then Invalid "--Choose number of rings between 1 and 10--"
                    else NewGame (read n)
                else Invalid "--b <numOfRings> needs a positive number between 1 and 10--"
   "z":[m]  -> if all isDigit m && read m > 0
                then MoveBack (read m)
                else Invalid "--z <numberOfTakeBacks> needs a positive number--"
   [x, y]   -> if all isDigit x && all isDigit y
                then Move (read x) (read y)
                else Invalid "--To move use two numbers between 1 and 3: <fromTower> <toTower>--"
   _ -> Invalid "--Unrecognized command--"



newGame :: Int -> IO()
newGame n = undefined --TODO

quit :: IO()
quit = do
    clearScreen
    return ()

move :: Board -> Int -> Int -> Board
move board f t =
    if legalMove board (f-1) (t-1)
        then move' board (f-1) (t-1) (getTopRing (board !! (f-1)))
        else board

move' :: Board -> Int -> Int -> Int -> Board
move' board f t r = [if f == i then fT else if t == i then tT else uT | i <- [0..2]]
    where
        fT = removeRing (board !! f)
        tT = addRing r (board !! t)
        uT = board !! unusedTower [0, 1, 2] f t

unusedTower :: [Int] -> Int -> Int -> Int
unusedTower (x:xs) t1 t2 =
    if x /= t1 && x /= t2
        then x
        else unusedTower xs t1 t2

legalMove :: Board -> Int -> Int -> Bool
legalMove board f t = (0 <= f && f <= 2) &&
                      (0 <= t && t <= 2) &&
                      (f /= t) &&
                      (getTopRing (board !! f) /= 0) &&
                      (getTopRing (board !! f) < getTopRing (board !! t) || getTopRing (board !! t) == 0)

getTopRing :: Tower -> Int
getTopRing [x] = x
getTopRing (x:xs) =
    if x == 0
        then getTopRing xs
        else x

removeRing :: Tower -> Tower
removeRing [] = []
removeRing (x:xs) =
    if x == 0
        then x : removeRing xs
        else 0 : xs

addRing :: Int -> Tower -> Tower
addRing r t = tail (addRing' r t)

addRing' :: Int -> Tower -> Tower
addRing' r [] = [r]
addRing' r (x:xs) =
    if x == 0
        then x : addRing' r xs
        else r : x : xs

moveBack :: Board -> [Board] -> Int -> Int -> Int -> IO() -- FIX
moveBack b [] s c _ = play b [] s c
moveBack b bs s c 0 = play b bs s c
moveBack b bs s c m = moveBack (last bs) (init bs) s (c-1) (m-1)

getMoveBackBoard :: Board -> [Board] -> Int -> Board
getMoveBackBoard b [] _ = b
getMoveBackBoard b bs 0 = b
getMoveBackBoard b bs m = getMoveBackBoard (last bs) (init bs) (m-1)

h :: IO()
h = undefined --TODO



showBoard :: Board -> IO()
showBoard b = putStrLn (unlines (makeBoardStr b))

startBoard :: Int -> Board
startBoard h = [makeTower h, makeTower 0, makeTower 0]

finished :: Board -> Int -> Bool
finished b s = b == endBoard s

endBoard :: Int -> Board
endBoard h = [makeTower 0, makeTower 0, makeTower h]

makeBoardStr :: Board -> [String]
makeBoardStr board = makeBoardStr' (map makeTowerStr board)

makeBoardStr' :: [[String]] -> [String]
makeBoardStr' ([]:_) = []
makeBoardStr' towerStrs = concatMap head towerStrs : makeBoardStr' (map tail towerStrs)

makeTowerStr :: Tower -> [String]
makeTowerStr = map makeRing

makeTower :: Int -> Tower
makeTower h =
    if h == 0
        then replicate 10 0
        else replicate (10 - h) 0 ++ [1 .. h]

makeRing :: Int -> String
makeRing size =
    if size == 0
        then "          |          "
        else replicate (10 - size + 1) ' ' ++ intersperse '-' (replicate size '#') ++ replicate (10 - size + 1) ' '