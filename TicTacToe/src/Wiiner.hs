{-# LANGUAGE OverloadedStrings #-}

import Data.List as List
import Data.Char

import Network.Wreq as Wreq
import Network.HTTP.Client
import Control.Exception as E
import Control.Lens
import Data.Either
import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy as LBStr

import Data.Monoid


data Move = Move {
    x :: Int
  , y :: Int
  , player :: Char
} deriving (Show, Eq)

type Moves = [Move]

-- #Movement

getColumnMoves :: Moves -> Int -> Moves -> Moves
getColumnMoves [] columNR storage = storage
getColumnMoves (mv:rest) columnNr storage
  | (x mv) == columnNr = getColumnMoves rest columnNr (mv:storage)
  | otherwise = getColumnMoves rest columnNr storage

getRowMoves :: Moves -> Int -> Moves -> Moves
getRowMoves [] columNR storage = storage
getRowMoves (mv:rest) columnNr storage
  | (y mv) == columnNr = getRowMoves rest columnNr (mv:storage)
  | otherwise = getRowMoves rest columnNr storage

getDiagonalMoves :: Moves -> Moves -> Moves
getDiagonalMoves [] storage = storage
getDiagonalMoves (mv:rest) storage
  | (((y mv) == 0) && ((x mv) == 0)) = getDiagonalMoves rest (mv:storage)
  | (((y mv) == 1) && ((x mv) == 1)) = getDiagonalMoves rest (mv:storage)
  | (((y mv) == 2) && ((x mv) == 2)) = getDiagonalMoves rest (mv:storage)
  | otherwise = getDiagonalMoves rest storage

getReverseDiagonalMoves :: Moves -> Moves -> Moves
getReverseDiagonalMoves [] storage = storage
getReverseDiagonalMoves (mv:rest) storage
  | (((y mv) == 0) && ((x mv) == 2)) = getReverseDiagonalMoves rest (mv:storage)
  | (((y mv) == 1) && ((x mv) == 1)) = getReverseDiagonalMoves rest (mv:storage)
  | (((y mv) == 2) && ((x mv) == 0)) = getReverseDiagonalMoves rest (mv:storage)
  | otherwise = getReverseDiagonalMoves rest storage

anyWinnersInColumnsOrRow :: Moves -> Maybe Char -> Maybe Char
anyWinnersInColumnsOrRow [] Nothing = Nothing
anyWinnersInColumnsOrRow (mv:moves) Nothing
  | List.length (mv:moves) < 3 = Nothing
  | otherwise = anyWinnersInColumnsOrRow moves (Just (player mv))
anyWinnersInColumnsOrRow (mv:moves) pl
  | ((Just(player mv)) /= pl) = Nothing
  | otherwise = anyWinnersInColumnsOrRow moves (Just (player mv))
anyWinnersInColumnsOrRow [] pl = pl

getAllColumnsAndRow :: Moves -> [Moves]
getAllColumnsAndRow moves =
  let
    firstCol = getColumnMoves moves 0 []
    secondCol = getColumnMoves moves 1 []
    thirdCol = getColumnMoves moves 2 []
    firstRow = getRowMoves moves 0 []
    secondRow = getRowMoves moves 1 []
    thirdRow = getRowMoves moves 2 []
    diag = getDiagonalMoves moves []
    reverseDiag = getDiagonalMoves moves []
  in
    (firstCol:secondCol:thirdCol:firstRow:secondRow:thirdRow:diag:reverseDiag:[])

getAllFinishedColsAndRows :: [Moves] -> [Moves] -> [Moves]
getAllFinishedColsAndRows [] storage = storage
getAllFinishedColsAndRows (moves:rest) storage
  | List.length moves > 2 = getAllFinishedColsAndRows rest (moves:storage)
  | otherwise = getAllFinishedColsAndRows rest storage


isCellTaken :: Moves -> (Int, Int) -> Bool
isCellTaken [] _ = False
isCellTaken (mv:rest) (cellX, cellY)
  | ((x mv) == cellX) && ((y mv) == cellY) = True
  | otherwise = isCellTaken rest (cellX,cellY)


getAllCells :: [(Int, Int)]
getAllCells = [ (x,y) | x<-[0..2], y<-[0..2] ]

getFreeCells :: Moves -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
getFreeCells moves [] storage = storage
getFreeCells moves (cell:rest) storage
  | isCellTaken moves cell = getFreeCells moves rest storage
  | otherwise = getFreeCells moves rest (cell:storage)

getDangerousCells :: Moves -> [(Int, Int)] -> Char -> [(Int, Int)] -> [(Int, Int)]
getDangerousCells moves [] _ buffer = buffer
getDangerousCells moves ((cellX, cellY):rest) enC buffer =
  let
    possibleMoves = ((Move cellX cellY enC):moves)
    isWinner = anyWinners (getAllFinishedColsAndRows (getAllColumnsAndRow possibleMoves) [])
  in
  case isWinner of
    Just enC -> getDangerousCells moves rest enC ((cellX, cellY):buffer)
    _ -> getDangerousCells moves rest enC buffer

-- Free cells -> Buffer -> List of all diagonal free cells
getFreeDiagonalCells :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
getFreeDiagonalCells [] buffer = buffer
getFreeDiagonalCells ((x,y):rest) buffer
  | ((x == 0) && (y == 0)) = getFreeDiagonalCells rest ((x,y):buffer)
  | ((x == 0) && (y == 2)) = getFreeDiagonalCells rest ((x,y):buffer)
  | ((x == 2) && (y == 0)) = getFreeDiagonalCells rest ((x,y):buffer)
  | ((x == 2) && (y == 2)) = getFreeDiagonalCells rest ((x,y):buffer)
  | otherwise = getFreeDiagonalCells rest buffer

-- freeCell -> [centerCell]
getCenterCell :: [(Int, Int)] -> [(Int, Int)]
getCenterCell [] = []
getCenterCell ((x,y):rest)
  | ((x == 1) && (y == 1)) = [(x,y)]
  | otherwise = getCenterCell rest

anyWinners :: [Moves] -> Maybe Char
anyWinners [] = Nothing
anyWinners (mvs:rest) =
  let
    possibleWinner = anyWinnersInColumnsOrRow mvs Nothing
  in
    if possibleWinner /= Nothing
      then possibleWinner
      else anyWinners rest

--                        dangerousCells -> [centerCell] -> diagCells   -> freeCells -> enemySymbol -> optimalMove
getMoveFromPossibleCells :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Char -> Move
getMoveFromPossibleCells ((cellX, cellY):_) _ _ _ pl = Move cellX cellY pl
getMoveFromPossibleCells [] ((cellX, cellY):_) _ _ pl = Move cellX cellY pl
getMoveFromPossibleCells [] [] ((cellX, cellY):_) _ pl = Move cellX cellY pl
getMoveFromPossibleCells [] [] [] ((cellX, cellY):_) pl = Move cellX cellY pl

-- #MovementEnd


getMySymbol :: Char -> Char
getMySymbol enC
  | enC == 'x' = 'o'
  | otherwise = 'x'

getSymbolFromList :: Moves -> Char
getSymbolFromList [] = 'o'
getSymbolFromList li = player (List.last li)

getSymbolByPlayer :: Char -> Char
getSymbolByPlayer '1' = 'x'
getSymbolByPlayer _ = 'o'

defend :: Moves -> Char -> Moves
defend (a:b:c:d:e:f:g:h:i:[]) _ = (a:b:c:d:e:f:g:h:i:[])  -- A very strange way, to check if all moves are done, as to not do the further calculations
defend moves mySymbol =
  let
    enC = getMySymbol mySymbol
    freeCells = getFreeCells moves getAllCells []
    dangerousCells = getDangerousCells moves freeCells enC []
    diagCells = getFreeDiagonalCells freeCells []
    centerCell = getCenterCell freeCells
    nextMove = getMoveFromPossibleCells dangerousCells centerCell diagCells freeCells mySymbol
  in
    (nextMove: moves)





-- #Http

httpGet :: String -> IO String
httpGet url = do
  let opts = defaults & header "Accept" .~ ["application/scala"]
  r <- getWith opts url
  return $ Char8.unpack $ LBStr.toStrict (r ^. Wreq.responseBody)

httpPost :: String -> String -> IO Int
httpPost url message = do
    let opts = defaults & header "Content-Type" .~ ["application/scala"]
    r <- postWith opts url $ Char8.pack message
    let result = r ^. Wreq.responseStatus . statusCode
    return result

baseUrl = "http://tictactoe.homedir.eu/game/"
buildUrl :: String -> String -> String
buildUrl gameId player =
    baseUrl ++
    gameId ++
    "/player/" ++
    player

-- #HttpEnd








-- #Parsing

parseTupleToMoves :: [(Int, Int, Char)] -> Moves -> Moves
parseTupleToMoves [] storage = storage
parseTupleToMoves ((x, y, v):rest) storage = parseTupleToMoves rest ((Move x y v):storage)


parseFullStringToTuple :: String -> [(Int, Int, Char)]
parseFullStringToTuple str = parseMovementsStringToTuple (removeFirstAndLastLetter (removeScalaCharacterFromString str))

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

removeFirstAndLastLetter :: [a] -> [a]
removeFirstAndLastLetter [] = []
removeFirstAndLastLetter [x] = []
removeFirstAndLastLetter xs = List.tail (List.init xs)

encodeMoves :: [Move] -> String -> String
encodeMoves [] buffer = buffer ++ ")"
encodeMoves (mv:res) [] =
  let
    listStr = "List(" ++ (encodeSingleMove mv)
    in encodeMoves res listStr
encodeMoves (mv:res) buffer =
  let
    mapStr = ", " ++ (encodeSingleMove mv)
    in encodeMoves res (buffer ++ mapStr)

encMoves :: Moves -> String -> String
encMoves moves str = str

encodeSingleMove :: Move -> String
encodeSingleMove move =
  let
    xStr = (show (x move))
    yStr = (show (y move))
    pStr = ([player move])
    valuesStr = "x -> " ++ xStr ++ ",   y  ->  " ++ yStr ++ ", v  ->   " ++ pStr --xStr ++ ", " ++ yStr ++ ", " ++pStr
    mapStr = "Map(" ++ valuesStr ++ ")"
    in mapStr

maybeCharToString :: Maybe Char -> String
maybeCharToString Nothing = "Nobody won this game"
maybeCharToString ch = (show ch) ++ " won this game"

-- #ParsingEnd







-- #Game

playGameAttack :: String -> String -> Moves -> IO (Maybe Char)
playGameAttack id pl moves =
    do
      if (winner /= Nothing) then return $ winner
      else
        if (List.length freeCells == 0) then return $ Nothing
        else
          do
            resId <- httpPost url (encodeMoves offense [])
            if (winnerPost /= Nothing || (List.length freeCellsPost == 0 )) then return $ winnerPost
            else do
              winner2 <- playGameDefend id pl
              return $ winner2
    where
      mySymbol = getSymbolByPlayer (List.head pl)
      offense = defend moves mySymbol
      url = buildUrl id pl
      freeCells = getFreeCells moves getAllCells []
      winner = anyWinners [moves]
      winnerPost = anyWinners [offense]
      freeCellsPost = getFreeCells offense getAllCells []

playGameDefend :: String -> String -> IO (Maybe Char)
playGameDefend id pl =
  do
    resMoves <- httpGet url
    res <- playGameAttack id pl (parseTupleToMoves (parseFullStringToTuple resMoves) [])
    return $ res
  where
    url = buildUrl id pl

innerStartGame :: String -> String -> IO (Maybe Char)
innerStartGame id "1" = playGameAttack id "1" []
innerStartGame id "2" = playGameDefend id "2"

-- #GameEnd
readGame :: String -> String -> IO String
readGame id pl =
  do
    winner <- innerStartGame id pl
    return $ maybeCharToString (winner)


-- #Main

--module Main where

--main :: IO ()
--main = Prelude.putStrLn "Hello, Haskell!"

-- #EndMain
