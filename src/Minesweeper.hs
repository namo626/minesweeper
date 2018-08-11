
module Minesweeper where

import Control.Lens
import System.Random (randomRs, mkStdGen)
import qualified Data.Map.Strict as M
import Data.List hiding (group)

type Pos = (Int, Int)
type Count = Int
type Size = Int

-- Doesn't have to be a square
data Grid = Grid
  { _numRows :: Size
  , _numCols :: Size
  , _container :: M.Map Pos Square
  , _gridPoss :: [[Pos]]
  }

numRows :: Lens' Grid Size
numRows = lens _numRows (\grid s -> grid { _numRows = s })

numCols :: Lens' Grid Size
numCols = lens _numCols (\grid s -> grid { _numCols = s })

container :: Lens' Grid (M.Map Pos Square)
container = lens _container (\grid s -> grid { _container = s })

gridPoss :: Lens' Grid [[Pos]]
gridPoss = lens _gridPoss (\grid s -> grid { _gridPoss = s })

instance Show Grid where
  show = showRows

data Square = EmptyC
            | EmptyO Int
            | BombC
            | BombO
  deriving (Eq)

instance Show Square where
  show EmptyC     = "E"
  show (EmptyO n) = show n
  show BombC      = "B"
  show BombO      = "X"

--data Status = Opened | Closed
 -- deriving (Eq, Show)

mkPoss :: Size -> Size -> [Pos]
mkPoss rows cols = [(i, j) | i <- rows', j <- cols']
  where
    rows' = [0 .. rows-1]
    cols' = [0 .. cols-1]

mkSquares :: Size -> [Square]
mkSquares size = replicate (size^2) (BombC)

mixed :: Size -> Size -> [Square]
mixed rows cols = replicate (len-5) EmptyC ++ replicate (5) BombC
  where
    len = rows * cols

randSquares :: Int -> Size -> Size -> [Square]
randSquares seed rows cols = map convert $ take len $ randomRs (1, 10) gen
  where
    gen = mkStdGen seed
    len = rows * cols
    convert :: Int -> Square
    convert n
      | n == 1 = BombC
      | otherwise = EmptyC

mkGrid :: Size -> Size -> (Size -> Size -> [Square]) -> Grid
mkGrid rows cols f = Grid {
  _numRows   = rows,
  _numCols   = cols,
  _container = M.fromList pairs,
  _gridPoss  = pss'
  }
  where
    pss   = mkPoss rows cols
    pss'  = group cols pss
    pairs = zip (pss) (f rows cols)

-- Accessing grid entities

squares :: Grid -> [Square]
squares = M.elems . view container

positions :: Grid -> [Pos]
positions = M.keys . view container

value :: Grid -> Pos -> Maybe Square
value grid pos = (view container grid) M.!? pos

value' :: Grid -> Pos -> Square
value' grid pos = (view container grid) M.! pos

mapWithKey :: (Pos -> Square -> Square) -> Grid -> Grid
mapWithKey f = over container (M.mapWithKey f)

-- replace a value at a specific key
replace :: Pos -> Square -> Grid -> Grid
replace pos sqr = over container (M.insert pos sqr)

groupSqrs :: Grid -> [[Square]]
groupSqrs grid = group rowLength $ squares grid
  where
    rowLength = view numCols grid

-- Displaying a grid
disp :: Grid -> IO ()
disp = putStrLn . showRows

showRow :: [Square] -> String
showRow = intercalate " | " . map show

convertRows :: Grid -> [String]
convertRows grid = map showRow $ group rowLength $ sqrs
  where
    sqrs      = squares grid
    rowLength = view numCols grid

showRows :: Grid -> String
showRows = ('\n':) . unlines . convertRows

group :: Int -> [a] -> [[a]]
group _ []    = []
group size xs = take size xs : group size (drop size xs)


-- | Return list of adjacent coordinates
adjacentPoss :: Size -> Size -> Pos -> [Pos]
adjacentPoss r c = pruneEdges r c . adjacents

adjacents :: Pos -> [Pos]
adjacents (r, c) = [(x, y) | x <- map ($r) range, y <- map ($c) range, (x, y) /= (r, c)]
  where
    range = [(-1 +), (0 +), (1 +)]

pruneEdges :: Size -> Size -> [Pos] -> [Pos]
pruneEdges r c = filter limits
  where
    limits (a, b) = a >= 0 && a < r && b >= 0 && b < c

-- | Count the number of adjacent bombs at a given coordinate in a grid
-- Index safety is guaranteed by adjacentPoss
bombCount :: Grid -> Pos -> Int
bombCount grid = length . filter isBomb . map (value' grid) . adjacentPoss r c
  where
    r              = view numRows grid
    c              = view numCols grid
    isBomb (BombC) = True
    isBomb _       = False


grid :: Grid
grid = mkGrid 10 8 (randSquares 0)


-- Playing the game


-- | Open a single cell, assuming the given position is valid
open :: Grid -> Pos -> Grid
open grid pos = case value' grid pos of
  BombC  -> replace pos (BombO) grid
  EmptyC -> replace pos (EmptyO (bombCount grid pos)) grid
  _      -> grid

openAll :: Grid -> Grid
openAll grid = foldl' open grid (positions grid)

-- | Recursively open a cell and its neighbours; a "turn"
-- Three cases can happen:
-- 1. the given position is already open --> leave the whole grid alone
-- 2. the closed position is empty && has no neighbouring bombs --> recursively open its neighbors
-- 3. the closed position is empty && has neighboring bombs --> open just that position
-- 4. the closed position is a bomb --> open just that position
move :: Grid -> Pos -> Grid
move grid pos =
  let sqr    = value' grid pos
      adjs   = adjacentPoss r c pos
      r      = view numRows grid
      c      = view numCols grid
      opened = open grid pos
  in case sqr of
    EmptyC   -> case bombCount grid pos of
                  0 -> foldl' move opened adjs
                  _ -> opened
    _        -> opened


-- | Player wins if there are no closed empty squares
wins :: Grid -> Bool
wins = not . any (== EmptyC) . view container

-- | Player loses if there's any open bomb
loses :: Grid -> Bool
loses = any (== BombO) . view container

data Result = Win | Lose | Neither
  deriving (Show, Eq)

checkResult :: Grid -> Result
checkResult grid
  | loses grid = Lose
  | wins grid  = Win
  | otherwise  = Neither


valid :: Grid -> Pos -> Bool
valid grid (i, j) = 0 <= i && i < r && 0 <= j && j < c
  where
    r = view numRows grid
    c = view numCols grid

play :: Grid -> IO ()
play grid = do
  print grid
  case checkResult grid of
    Win     -> putStrLn "You won."
    Lose    -> putStrLn "You lost."
    Neither -> do
      i <- read `fmap` getLine :: IO Int
      j <- read `fmap` getLine :: IO Int
      if valid grid (i, j) then do
        let grid' = move grid (i, j)
        play grid'
      else do
        putStrLn "Invalid position"
        play grid
