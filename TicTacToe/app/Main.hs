module Main where

import FPTicTacToeAS.Game

main :: IO ()
main = do
    putStrLn "Enter game id:"
    gameId <- getLine
    putStrLn "Select player id (1 or 2):"
    playerId <- readInput "1" "2" "Invalid player, try again!"
    winner <- readGame gameId playerId
    putStrLn winner
    where
        readInput val1 val2 errorMessage  = do
            result <- getLine
            if (result /= val1) && (result /= val2)
            then do
                putStrLn errorMessage
                readInput val1 val2 errorMessage
            else
                return result
