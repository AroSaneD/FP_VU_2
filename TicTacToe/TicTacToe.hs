import Data.List
import Data.Char
import Data.Monoid


--  Recieve input form console. (apparently not needed, ignore for now)
validateTicTacToeGame :: IO String
validateTicTacToeGame =
    do
      putStrLn "Input the encoded movements you wish to validate."
      movements <- getLine
      return $ getEvaluationOfMovementString movements


--  Evaluate string (for console output / cleaner reprisentation)
getEvaluationOfMovementString :: [Char] -> String
getEvaluationOfMovementString str =
    let
      isStringValid = isGivenInputValid str
      in
      case isStringValid of
        True -> "The movements and turns in the given string are VALID."
        _    -> "The movements and turns in the given string are NOT VALID."


--  Validate movements
isGivenInputValid :: [Char] -> Bool
isGivenInputValid msg =
    let
      a = removeFirstAndLastLetter (removeScalaCharacterFromString msg)
      movementTuples = parseMovementsStringToTuple a
      validTurns     = arePlayerTurnsValid movementTuples '_'
      validMovements = areMovementsValid movementTuples
    in
      (validTurns && validMovements)


areMovementsValid :: [(Int, Int, Char)] -> Bool
areMovementsValid [] = True
areMovementsValid (c:p)
    | arePreviousMovementsValid p c = areMovementsValid p
    | otherwise = False


arePreviousMovementsValid :: [(Int, Int, Char)] -> (Int, Int, Char) -> Bool
arePreviousMovementsValid [] _ = True
arePreviousMovementsValid ((px, py, pv):t) (cx, cy, cv)
    | (px == cx) && (py == cy) = False
    | otherwise = arePreviousMovementsValid t (cx, cy, cv)


--  Validate turns
arePlayerTurnsValid :: [(Int, Int, Char)] -> Char -> Bool
arePlayerTurnsValid [] _ = True
arePlayerTurnsValid ((_, _, currentPlayer):rest) lastPlayer
    | currentPlayer == lastPlayer = False
    | otherwise = arePlayerTurnsValid rest currentPlayer


--  Parse single scala map string to tuple
parseMovementsStringToTuple :: [Char] -> [(Int, Int, Char)]   -- Possible to (tail call) optimize this. Add an argument and keep adding to the argument
parseMovementsStringToTuple [] = []
parseMovementsStringToTuple ('(':'x':x:',':'y':y:',':'v':v:')':rest) = ((digitToInt x, digitToInt y, v) : (parseMovementsStringToTuple rest))
parseMovementsStringToTuple (',':'(':'x':x:',':'y':y:',':'v':v:')':rest) = ((digitToInt x, digitToInt y, v) : (parseMovementsStringToTuple rest))


--  Remove scala words
removeScalaCharacterFromString :: [Char] -> [Char]
removeScalaCharacterFromString [] = []
removeScalaCharacterFromString ('L':'i':'s':'t':b) = removeScalaCharacterFromString b
removeScalaCharacterFromString ('M':'a':'p':b) = removeScalaCharacterFromString b
removeScalaCharacterFromString ('-':'>':b) = removeScalaCharacterFromString b
removeScalaCharacterFromString (' ':b) = removeScalaCharacterFromString b
removeScalaCharacterFromString (a:b) = a : removeScalaCharacterFromString b


--  Remove opening and closing paranthesis, for easier navigation
removeFirstAndLastLetter :: [a] -> [a]
removeFirstAndLastLetter [] = []
removeFirstAndLastLetter [x] = []
removeFirstAndLastLetter xs = tail (init xs)
