module Main where

import Minesweeper (grid, Square(..))
import UI (app, Game(..), drawSquare)
import Brick (defaultMain, simpleMain, str, getVtyHandle)
import qualified Graphics.Vty as V
import Control.Monad


main :: IO ()
main = do
  let initGame = Game { _gameGrid = grid }
  defaultMain app initGame
  return ()
