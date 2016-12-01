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

--httpGet :: String -> IO String
--httpGet url = do
    --let opts = defaults & header "Accept" .~ ["application/bencode+map"]
    --r <- (Right <$> getWith opts url) `E.catch` httpExceptionHandler
    --return $ getValue r
    --where
        --getValue :: Either String (Wreq.Response LBStr.ByteString) -> String
        --getValue (Left statusMessage) = statusMessage
        --getValue (Right response) = Char8.unpack $ LBStr.toStrict (response ^. Wreq.responseBody)

--httpExceptionHandler (StatusCodeException s _ _) = return $ Left $ Char8.unpack (s ^. statusMessage)
testMove1 :: String
testMove1 = "[{\"x\": 0, \"y\":  1,   \"v\":  \"x\"}]"

testMove2 :: String
testMove2 = "[{\"x\": 0, \"y\":  1,   \"v\":  \"x\"}, {\"x\":  1,   \"y\":  1,  \"v\":   \"o\"}]"

testMove25 :: String
testMove25 = "[{\"x\":  1,   \"y\":  1,  \"v\":   \"o\"}]"

httpGet :: String -> IO String
httpGet url = do
  let opts = defaults & header "Accept" .~ ["application/scala"]
  r <- getWith opts url
  return $ Char8.unpack $ LBStr.toStrict (r ^. Wreq.responseBody)

httpPost :: String -> String -> IO Int
httpPost url message = do
    let opts = defaults & header "Content-Type" .~ ["application/json"]
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

buildUrl2 :: String -> String
buildUrl2 pl = buildUrl "acer3123" pl

testPost1 :: IO Int
testPost1 = httpPost (buildUrl2 "1") testMove1

testGet2 :: IO String
testGet2 = httpGet (buildUrl2 "2")

testGet25 :: IO [(Int, Int, Char)]
testGet25 =
  do
    mv <- testGet2
    return $ parseFullStringToTuple mv

returnAString :: String -> String
returnAString a = "hello"

testPost2 :: IO Int
testPost2 = httpPost (buildUrl2 "2") testMove2

testPost25 :: IO Int
testPost25 = httpPost (buildUrl2 "2") testMove25

testGet3 :: IO String
testGet3 = httpGet (buildUrl2 "1")

testResponse :: String
testResponse = "List(Map(x -> 0, y -> 1, v -> x))";


-- #Parsing
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
-- #ParsingEnd
