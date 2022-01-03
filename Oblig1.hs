-- William Amundsen Winterstø
module Oblig1 where

dictionary = [
        ("bb",["Big Brother"]),
        ("dep",["department"]),
        ("sec", ["Sector"]),
        ("doubleplusgood",["excellent", "fabulous", "fantastic", "best"]),
        ("doubleplusungood", ["terrible", "horrible", "worst"]),
        ("Ingsoc", ["English Socialism"]),
        ("joycamp", ["labour camp"]),
        ("Oldspeak", ["Standard English", "English"]),
        ("oldthink", ["objectivity", "rationalism", "democracy"]),
        ("thinkpol", ["The Thought Police"]),
        ("prolefeed", ["Popular culture", "pop-culture"]),
        ("crimethink", ["liberty", "equality", "privacy", "thoughtcrime"]),
        ("fullwise", ["fully", "completely", "totally"]),
        ("goodthink", ["political orthodoxy", "politically orthodox thought", "orthodox thought"]),
        ("goodwise", ["well"]),
        ("ownlife", ["anti-social tendency", "solitude", "individualism"]),
        ("plusgood", ["very good", "great"]),
        ("plusungood", ["very bad"]),
        ("misprint", ["error", "misprediction"]),
        ("Miniluv", ["The Ministry of Love"]),
        ("Minipax", ["The Ministry of Peace"]),
        ("Minitrue", ["The Ministry of Truth"]),
        ("Miniplenty", ["The Ministry of Plenty"]),
        ("bellyfeel", ["blind, enthusiastic acceptance"]),
        ("doublethink", ["believing two contradictory ideas"]),
        ("duckspeak", ["vocal support of political orthodoxies"]),
        ("un", ["not"]),
        ("peace", ["war"]),
        ("strength", ["ignorance"]),
        -- The next line contains a list of forbidden words that don't have a translation to Newspeak, these should be replaced with '*'s
        ("",["freedom", "revolution", "fun", "diary", "surveillance", "Great Britain", "Winston Smith", "Julia"])
        ]


-- Oppgave 1 ----------------------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix pref str = (head pref == head str) && isPrefix (tail pref) (tail str)


-- Oppgave 2 ----------------------------------------------------
locate :: String -> String -> [(Int,Int)]
locate [] str = []
locate pref str = findIndexes pref str 0

findIndexes :: String -> String -> Int -> [(Int,Int)]
findIndexes pref [] i = []
findIndexes pref str i =
    if isPrefix pref str
        then (i, i + length pref) : findIndexes pref (tail str) (i+1)
        else findIndexes pref (tail str) (i+1)


-- Oppgave 3 ----------------------------------------------------
translate :: String -> String
translate [] = []
translate word = findTranslation word dictionary

findTranslation :: String -> [(String, [String])] -> String
findTranslation word [] = []
findTranslation word dic =
    if word `elem` snd (head dic)
        then fst (head dic)
        else findTranslation word (tail dic)


-- Oppgave 4 ----------------------------------------------------
replace :: [(Int,Int)] -> String -> String
replace [] str = str
replace tuples str = replaceSentence (qsortrev tuples) str

qsortrev :: Ord a => [a] -> [a]
qsortrev [] = []
qsortrev (x : xs) = qsortrev larger ++ [x] ++ qsortrev smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

replaceSentence :: [(Int,Int)] -> String -> String
replaceSentence tuples str = foldl replaceWordAtThisTuple str tuples

replaceWordAtThisTuple :: String -> (Int,Int) -> String
replaceWordAtThisTuple str (i,j) = take i str ++ replaceWord (drop i (take j str)) ++ drop j str

replaceWord :: String -> String
replaceWord word =
    if null (translate word)
        then ['*'| x <- word]
        else translate word


-- Oppgave 5 ----------------------------------------------------
toNewspeak :: String -> String
toNewspeak str = newSpeakTranslator str dictionary

newSpeakTranslator :: String -> [(String, [String])] -> String
newSpeakTranslator str [] = str
newSpeakTranslator str dic = newSpeakTranslator (implementThisNewspeakWord str (head dic)) (tail dic)

implementThisNewspeakWord :: String -> (String, [String]) -> String
implementThisNewspeakWord str (newspeak, []) = str
implementThisNewspeakWord str (newspeak, oldspeak) =
    if locate (head oldspeak) str /= []
        then implementThisNewspeakWord (updatedString str (head oldspeak)) (newspeak, tail oldspeak)
        else implementThisNewspeakWord str (newspeak, tail oldspeak)

updatedString :: String -> String -> String
updatedString str oldspeak = replace (locate oldspeak str) str


-- Oppgave 6 ----------------------------------------------------
analytics :: String -> String -> Int
analytics original translated = findPercent (numberOfChangedCharacters original translated 0) (length original)

numberOfChangedCharacters :: String -> String -> Int -> Int
numberOfChangedCharacters [] translated n = n
numberOfChangedCharacters original translated n =
    if locate (take 1 original) translated /= []
        then numberOfChangedCharacters (tail original) (removeFirstInstranceOfCharacter (locate (take 1 original) translated) translated) n
        else numberOfChangedCharacters (tail original) translated (n+1)

removeFirstInstranceOfCharacter :: [(Int,Int)] -> String -> String
removeFirstInstranceOfCharacter (x:xs) str = take (fst x) str ++ drop (snd x) str

findPercent :: Int -> Int -> Int 
findPercent x y = floor (100 * ( a / b ) + 1/2)
-- The numbers are turned into floats so we can round correctly. The round function rounds to closest even number.
-- This makes a/b always equal 0 since it is a number between 0 and 1
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float


-- Oppgave 7 ----------------------------------------------------
main :: String -> (String, Int)
main oldspeak = (toNewspeak oldspeak, analytics oldspeak (toNewspeak oldspeak))