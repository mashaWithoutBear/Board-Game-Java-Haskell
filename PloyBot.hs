--- module (NICHT AENDERN!)
module PloyBot where

import Data.Char
import Util
import Data.List
import Data.Maybe

--- external signatures (NICHT AENDERN!)
getMove :: String -> String
listMoves :: String -> String

--- YOUR IMPLEMENTATION STARTS HERE ---
--- Group work by Hendrik Schulze Bröring, Mariia Kucheiko, Theresa Kuhn
getMove s= tail(take 8 (listMoves s))
listMoves xs= buildString (dealWithString xs) (isWhite xs) 

dealWithString::String->[String]
dealWithString xs = zipWith (++)["19","29","39","49","59","69","79","89","99","18","28","38","48","58","68","78","88","98","17","27","37","47","57","67","77","87","97","16","26","36","46","56","66","76","86","96","15","25","35","45","55","65","75","85","95","14","24","34","44","54","64","74","84","94","13","23","33","43","53","63","73","83","93","12","22","32","42","52","62","72","82","92","11","21","31","41","51","61","71","81","91"]  (prepareString xs)

prepareString::String->[String]
prepareString xs=  concat (splitCol (splitRow (cutOff xs)))

splitRow a= splitOn "/" a
splitCol a = map (splitOn ",") a

cutOff::String -> String
cutOff xs= init(init xs) --schneidet " b" ab

--letztes Element rausnehmen->player
isWhite:: String->Bool
isWhite xs
    |[(last xs)]=="w"= True
    |otherwise = False

buildString::[String]->Bool->String 
buildString a b 
   |null (concat (map (++",") (writeMoves a b))) = "[]"
   |otherwise = "["++init( concat (map (++",") (writeMoves a b)))++"]" --init schneidet die letzte Komma weg. TODO:Fehlerbehandlung. "Maybe"??? Liste kann leer sein

writeMoves::[String]->Bool->[String] --kriegt Liste ["w84",..] und den Player "w" oder "b" und giebt die Liste von Strings (Moves) aus
writeMoves a b =concat( map (computeMoves (Board (buildField a) b)) (filterN (buildField a) b)) --Für jedes Element der Liste ["w84",..] tue was; kommt automatisch eine Liste raus

filterN::[Figure]->Bool->[Figure]
filterN list b = filter (\figure-> (maxSteps (figure)/=(-1))&&(color (figure)==b)) list

computeMoves::Board->Figure->[String]
computeMoves board figure= (++) (map (buildMove figure) (nichtMehrHendrik board figure)) (map((++) (buildSameStartEnd figure)) ["0","1","2","3","4","5","6","7"])

nichtMehrHendrik::Board->Figure->[[Int]] --liefert valide Endpositionen für die Figur in alle möglichen Richtungen, z.B.[[1,1],[2,2]]
nichtMehrHendrik b f= concat (map (somethingOnTheWay f b) (allPossibleMoves f))



somethingOnTheWay::Figure->Board->[[Int]]->[[Int]] -- foo wird für 1 Richtung angewendet, liefert alle valide Positionen
somethingOnTheWay _ _ [] = [] --am Ende der Liste der Positionen gelandet->Alle Positionen waren frei, alle Moves valide->Abbruch
somethingOnTheWay f b (l:list)
   |(herFigureOnThisPosition (figures(b)) (isWhitePlayer(b)) l)==True = [l]++(somethingOnTheWay f b [])--eine fremde Figur steht auf dem Ziel->kann geschlagen werden->Ziel valid, aber weiter darf man nicht laufen, weil es überspringen wäre
   |(myFigureOnThisPosition (figures(b)) (isWhitePlayer(b)) l)==True = somethingOnTheWay f b [] -- eine eigene Figur steht auf dem Feld-> Ziel nicht valid->Abbruch
   |otherwise = [l]++(somethingOnTheWay f b list) --nichts steht auf dem Ziel->Ziel valid->schaue die nächsten an

myFigureOnThisPosition:: [Figure]->Bool->[Int]->Bool --[1,2] ist eine Position
myFigureOnThisPosition figures pl i= foldr (\f acc -> (acc||( col(f)==(i!!0) && row(f)==(i!!1) && maxSteps(f)/=(-1) && color(f)==pl ) ) ) False figures

herFigureOnThisPosition:: [Figure]->Bool->[Int]->Bool
herFigureOnThisPosition figures pl i= foldr ( \f acc -> ( acc||(  col(f)==(i!!0) && row(f)==(i!!1) && maxSteps(f)/=(-1)&& (color(f))/=pl ) ) ) False figures
 ---------------------------------
rotationWithSteps::Figure -> [String]
rotationWithSteps f
   |isShield (intRepresentation(f)) = ["0,","1,","2,","3,","4,","5,","6,","7"]
   |otherwise = ["0"]

buildSameStartEnd::Figure -> String
buildSameStartEnd figure =
   [getAscii(col(figure))] ++ [intToChar(row(figure))]++"-"++[getAscii(col(figure))] ++ [intToChar(row(figure))] ++ "-"

buildMove::Figure->[Int]->String --[1,2]
buildMove _ [] = []
buildMove f st= concat(map ((++) ([(getAscii (col(f)))]++[intToChar(row(f))]++"-" ++[getAscii(st!!0)]++[intToChar(st!!1)]++ "-"))(rotationWithSteps f))
-------------------------------------------

buildField::[String]->[Figure]
buildField a= map createFigureFromString a

data Board = Board {figures::[Figure],isWhitePlayer::Bool} deriving Show 

test3= splitRow ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69, b" 
test4= dealWithString ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69, b"
test5= listMoves ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69, b"
 
testFigures= buildField test4




isCommander c = c `elem` [170, 85]
isLance l = l `elem` [131, 7, 14, 28, 56, 112, 224, 193, 146, 37, 74, 148, 41, 82, 164, 73, 69, 138, 21, 42, 84, 168, 81, 162]
isProbe p = p `elem` [17, 34, 68, 136, 130, 5, 10, 20, 40, 80, 160, 65, 129, 3, 6, 12, 24, 48, 96, 192]
isShield s= s `elem` [1, 2, 4, 8, 16, 32, 64, 128]

data Figure = Commander {color:: Bool, intRepresentation:: Int, col:: Int, row::Int, maxSteps::Int}
      | Lance {color:: Bool, intRepresentation:: Int, col:: Int, row::Int, maxSteps::Int}
      | Probe {color:: Bool, intRepresentation:: Int,  col:: Int, row::Int, maxSteps::Int}
      | Shield {color:: Bool, intRepresentation:: Int, col:: Int, row::Int, maxSteps::Int}
      | None {col:: Int, row::Int, maxSteps::Int}
	deriving Show


createFigureFromString::[Char]->Figure
createFigureFromString (col:row:f)
  |null f = (None (charToInt col) (charToInt row) (-1))
  |isCommander (changeToInt (tail f)) = (Commander (getFigureColor (f!!0)) (changeToInt (tail f)) (charToInt col) (charToInt row) 1)
  |isLance (changeToInt (tail f)) = (Lance (getFigureColor (f!!0)) (changeToInt (tail f)) (charToInt col) (charToInt row) 3)
  |isProbe (changeToInt (tail f)) = (Probe (getFigureColor (f!!0)) (changeToInt (tail f)) (charToInt col) (charToInt row) 2)
  |isShield (changeToInt (tail f))= (Shield (getFigureColor (f!!0)) (changeToInt (tail f)) (charToInt col) (charToInt row) 1)
  

changeToInt::String -> Int
changeToInt a = read a ::Int

charToInt::Char -> Int
charToInt a = digitToInt a

getAscii::Int -> Char
getAscii a = chr(a+96)

intToChar::Int->Char
intToChar a= intToDigit a

getFigureColor :: Char -> Bool 
getFigureColor c 
  |c == 'w' = True
  |c == 'b' = False
-- /home/mascha/Dokumente/SWTPP/HA/BotVorgabe
--Input: Integer || Output: Rin Array, das die binäre Repräsentation (reversed) desselben Integers darstellt.

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n 
        | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

--Input: Integer || Output: Array, das die Indizes der Einsen von der Binärdarstellung des Integers ausgibt. zB 7 = 00000111 im Binärsystem => Outputarray = [0,1,2]
toInd :: Int -> [Int]
toInd n = findIndices (==1) (reverse (toBin n))

-------------------------------------------------------------------

-- Input: Figur, Max_Steps || Output: Alle Felder [Int, Int] auf die in eine Richtung gezogen werden darf.

calculateMoveUp :: Figure -> Int -> [Int] -> [[Int]]
calculateMoveUp f 0 [] = []
calculateMoveUp f a [] = filter (\x -> (x!!0 > 0 && x!!0 < 10) && (x!!1 > 0 && x!!1 < 10))(reverse([[col(f), row(f) + a]] ++ reverse(calculateMoveUp f (a-1) [])))

calculateMoveRightUp :: Figure -> Int -> [Int] -> [[Int]]
calculateMoveRightUp f 0 [] = []
calculateMoveRightUp f a [] = filter (\x -> (x!!0 > 0 && x!!0 < 10) && (x!!1 > 0 && x!!1 < 10))(reverse([[col(f) + a, row(f) + a]] ++ reverse(calculateMoveRightUp f (a-1) [])))

calculateMoveRight :: Figure -> Int -> [Int] -> [[Int]]
calculateMoveRight f 0 [] = []
calculateMoveRight f a [] = filter (\x -> (x!!0 > 0 && x!!0 < 10) && (x!!1 > 0 && x!!1 < 10))(reverse([[col(f) + a, row(f)]] ++ reverse(calculateMoveRight f (a-1) [])))

calculateMoveRightDown :: Figure -> Int -> [Int] -> [[Int]]
calculateMoveRightDown f 0 [] = []
calculateMoveRightDown f a [] = filter (\x -> (x!!0 > 0 && x!!0 < 10) && (x!!1 > 0 && x!!1 < 10))(reverse([[col(f) + a, row(f) - a]] ++ reverse(calculateMoveRightDown f (a-1) [])))

calculateMoveDown :: Figure -> Int -> [Int] -> [[Int]]
calculateMoveDown f 0 [] = []
calculateMoveDown f a [] = filter (\x -> (x!!0 > 0 && x!!0 < 10) && (x!!1 > 0 && x!!1 < 10))(reverse([[col(f), row(f) - a]] ++ reverse(calculateMoveDown f (a-1) [])))

calculateMoveLeftDown :: Figure -> Int -> [Int] -> [[Int]]
calculateMoveLeftDown f 0 [] = []
calculateMoveLeftDown f a [] = filter (\x -> (x!!0 > 0 && x!!0 < 10) && (x!!1 > 0 && x!!1 < 10))(reverse([[col(f) - a, row(f) - a]] ++ reverse(calculateMoveLeftDown f (a-1) [])))

calculateMoveLeft :: Figure -> Int -> [Int] -> [[Int]]
calculateMoveLeft f 0 [] = []
calculateMoveLeft f a [] = filter (\x -> (x!!0 > 0 && x!!0 < 10) && (x!!1 > 0 && x!!1 < 10))(reverse([[col(f) - a, row(f)]] ++ reverse(calculateMoveLeft f (a-1) [])))

calculateMoveLeftUp :: Figure -> Int -> [Int] -> [[Int]]
calculateMoveLeftUp f 0 [] = []
calculateMoveLeftUp f a [] = filter (\x -> (x!!0 > 0 && x!!0 < 10) && (x!!1 > 0 && x!!1 < 10))(reverse([[col(f) - a, row(f) + a]] ++ reverse(calculateMoveLeftUp f (a-1) [])))


--Input: Figur, Richtungsangabe (Int) 0 = oben, 2 = rechts usw. || Output: Liste von möglichen Zügen für eine Richtung

possibleMoves :: Figure -> Int -> [[Int]]
possibleMoves f a
        | a == 0 = calculateMoveUp f (maxSteps(f)) []
        | a == 1 = calculateMoveRightUp f (maxSteps(f)) []
        | a == 2 = calculateMoveRight f (maxSteps(f)) []
        | a == 3 = calculateMoveRightDown f (maxSteps(f)) []
        | a == 4 = calculateMoveDown f (maxSteps(f)) []
        | a == 5 = calculateMoveLeftDown f (maxSteps(f)) []
        | a == 6 = calculateMoveLeft f (maxSteps(f)) []
        | a == 7 = calculateMoveLeftUp f (maxSteps(f)) []

-- Input: Figur, Array mit Richtungsabgaben || Output: Alle möglichen der Figur

allPossibleMoves :: Figure -> [[[Int]]]
allPossibleMoves f = map (possibleMoves(f)) (toInd(intRepresentation(f))) --arr

