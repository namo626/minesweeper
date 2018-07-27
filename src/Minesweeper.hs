module Minesweeper where

import qualified Data.Map.Strict as M

type Pos = (Int, Int)
type Grid = M.Map Pos Square
type Count = Int
type Size = Int

data Square = Bomb Status | Empty Status Count
  deriving (Eq, Show)
data Status = Opened | Closed
  deriving (Eq, Show)

mkPoss :: Size -> [Pos]
mkPoss size = [(i, j) | i <- range, j <- range]
  where range = [0 .. size-1]

mkSquares :: Size -> [Square]
mkSquares size = replicate (size^2) (Bomb Closed)

mixed :: Size -> [Square]
mixed size = (replicate (dim-5) (Empty Closed 0)) ++ (replicate 5 (Bomb Closed))
  where dim = size^2

mkGrid :: Size -> (Size -> [Square]) -> Grid
mkGrid size f = M.fromList pairs
  where pairs = zip (mkPoss size) (f size)

-- Accessing grid entities
squares :: Grid -> [Square]
squares = M.elems

positions :: Grid -> [Pos]
positions = M.keys

value :: Grid -> Pos -> Maybe Square
value = (M.!?)



-- Displaying a grid
display :: Grid -> IO ()
display grid = mapM_ print $ group size $ sqs
  where
    sqs  = squares grid
    ps   = positions grid
    size = 1 + maxRow ps

maxRow :: [Pos] -> Int
maxRow = maximum . map fst

group :: Int -> [a] -> [[a]]
group _ []    = []
group size xs = take size xs : group size (drop size xs)


-- Filling each cell with number of adjacent bombs
adjacentPoss :: Size -> Pos -> [Pos]
adjacentPoss size = pruneEdges size . adjacents

adjacents :: Pos -> [Pos]
adjacents (i, j) = [(x, y) | x <- map ($i) range, y <- map ($j) range, (x, y) /= (i, j)]
  where
    range = [(-1 +), (0 +), (1 +)]

pruneEdges :: Size -> [Pos] -> [Pos]
pruneEdges size = filter limits
  where
    limits (a, b) = a >= 0 && a < size && b >= 0 && b < size

bombCount :: Size -> Grid -> Pos -> Int
bombCount size grid = length . filter isBomb . map (grid M.!) . adjacentPoss size
  where
    isBomb (Bomb _) = True
    isBomb _        = False

update :: Int -> Square -> Square
update n (Empty x 0) = Empty x n
update _ e           = e

update' :: Size -> Grid -> (Pos -> Square -> Square)
update' size grid = update . bombCount size grid

bombCounts :: Size -> Grid -> Grid
bombCounts size grid = M.mapWithKey (update' size grid) grid

grid :: Grid
grid = mkGrid 5 mixed
