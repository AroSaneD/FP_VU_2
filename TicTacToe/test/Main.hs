import Test.Hspec
import Prelude
import Data.Monoid
import Control.Monad
import FPTicTacToeAS.Game

main :: IO ()
main = hspec $ do
  describe "TicTacToe.Actions.Winner" $ do

    it "The first move, if no prior moves, will be in the center" $ do
      defend [] 'x' `shouldBe` [(Move 1 1 'x')]

    it "The first player should always be the x" $ do
      getSymbolByPlayer '1' `shouldBe` 'x'

    it "Moves should be correctly parsed into scala" $ do
      encodeSingleMove (Move 1 1 'x') `shouldBe` "Map(x -> 1,   y  ->  1, v  ->   x)"

    it "The algorithm should react instantly to situations, where it would lose" $ do
      defend [Move 0 0 'x', Move 0 2 'x'] 'o' `shouldBe` [Move 0 1 'o', Move 0 0 'x', Move 0 2 'x']

    it "The algorithm should correctly assess the winner of a board (1)" $ do
      anyWinnersInBoard [Move 0 0 'x', Move 0 1 'x', Move 0 2 'y'] `shouldBe` Nothing

    it "The algorithm should correctly assess the winner of a board (2)" $ do
      anyWinnersInBoard [] `shouldBe` Nothing

    it "The algorithm should correctly assess the winner of a board (3)" $ do
      anyWinnersInBoard [Move 0 0 'x', Move 0 1 'x', Move 0 2 'y', Move 2 2 'x', Move 1 1 'x'] `shouldBe` Just 'x'

    

    it "always returns true" $ do
      True `shouldBe` True
