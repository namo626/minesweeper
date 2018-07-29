
module Minesweeper where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.List hiding (group)

type Pos = (Int, Int)
type Count = Int
type Size = Int

data Grid = Grid
  { _gridSize :: Size
  , _container :: M.Map Pos Square
  }

gridSize :: Lens' Grid Size
gridSize = lens _gridSize (\grid s -> grid { _gridSize = s })

container :: Lens' Grid (M.Map Pos Square)
container = lens _container (\grid s -> grid { _container = s })

instance Show Grid where
  show = showRows

data Square = EmptyC
            | EmptyO Int
            | BombC
            | BombO
  deriving (Eq)

instance Show Square where
  show EmptyC     = "O"
  show (EmptyO n) = show n
  show BombC      = "B"
  show BombO      = "X"

--data Status = Opened | Closed
 -- deriving (Eq, Show)

mkPoss :: Size -> [Pos]
mkPoss size = [(i, j) | i <- range, j <- range]
  where range = [0 .. size-1]

mkSquares :: Size -> [Square]
mkSquares size = replicate (size^2) (BombC)

mixed :: Size -> [Square]
mixed size = (replicate (dim-5) (EmptyC)) ++ (replicate 5 (BombC))
  where dim = size^2

mkGrid :: Size -> (Size -> [Square]) -> Grid
mkGrid size f = Grid {
  _gridSize  = size,
  _container = M.fromList pairs
  }
  where pairs = zip (mkPoss size) (f size)

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


-- Displaying a grid
disp :: Grid -> IO ()
disp = putStrLn . showRows

showRow :: [Square] -> String
showRow = intercalate " | " . map show

showRows :: Grid -> String
showRows grid = ('\n':) . unlines . map showRow . group size $ sqrs
  where
    sqrs = squares grid
    pss  = positions grid
    size = view gridSize grid

group :: Int -> [a] -> [[a]]
group _ []    = []
group size xs = take size xs : group size (drop size xs)


-- Filling each cell with number of adjacent bombs
adjacentPoss :: Size -> Pos -> [Pos]
adjacentPoss size = pruneEdges size . adjacents

adjacents :: Pos -> [Pos]
adjacents (r, c) = [(x, y) | x <- map ($r) range, y <- map ($c) range, (x, y) /= (r, c)]
  where
    range = [(-1 +), (0 +), (1 +)]

pruneEdges :: Size -> [Pos] -> [Pos]
pruneEdges size = filter limits
  where
    limits (a, b) = a >= 0 && a < size && b >= 0 && b < size

-- | Count the number of adjacent bombs at a given coordinate in a grid
-- Index safety is guaranteed by adjacentPoss
bombCount :: Grid -> Pos -> Int
bombCount grid = length . filter isBomb . map (value' grid) . adjacentPoss size
  where
    size           = view gridSize grid
    isBomb (BombC) = True
    isBomb _       = False

-- Open a square and update its status accordingly
-- open :: Square -> Square
-- update n (EmptyC) = EmptyO n
-- update _ BombC    = BombO
-- update _ x        = x

-- update' :: Grid -> (Pos -> Square -> Square)
-- update' grid = update . bombCount grid

-- bombCounts :: Grid -> Grid
-- bombCounts grid = mapWithKey (update' grid) grid

grid :: Grid
grid = mkGrid 5 mixed


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
      adjs   = adjacentPoss size pos
      size   = view gridSize grid
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
