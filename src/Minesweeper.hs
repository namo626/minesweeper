module Minesweeper where

import qualified Data.Map.Strict as M

type Pos = (Int, Int)
type Count = Int
type Size = Int

data Grid = Grid
  { gridSize :: Size
  , container :: M.Map Pos Square
  }

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
mkGrid size f = Grid {
  gridSize = size,
  container = M.fromList pairs
  }
  where pairs = zip (mkPoss size) (f size)

-- Accessing grid entities
squares :: Grid -> [Square]
squares = M.elems . container

positions :: Grid -> [Pos]
positions = M.keys . container

value :: Grid -> Pos -> Maybe Square
value grid pos = (container grid) M.!? pos

value' :: Grid -> Pos -> Square
value' grid pos = (container grid) M.! pos

mapWithKey :: (Pos -> Square -> Square) -> Grid -> Grid
mapWithKey f grid = grid { container = M.mapWithKey f . container $ grid }



-- Displaying a grid
display :: Grid -> IO ()
display grid = mapM_ print $ group size $ sqs
  where
    sqs  = squares grid
    ps   = positions grid
    size = gridSize grid

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

bombCount :: Grid -> Pos -> Int
bombCount grid = length . filter isBomb . map (value' grid) . adjacentPoss size
  where
    size = gridSize grid
    isBomb (Bomb _) = True
    isBomb _        = False

update :: Int -> Square -> Square
update n (Empty x 0) = Empty x n
update _ e           = e

update' :: Grid -> (Pos -> Square -> Square)
update' grid = update . bombCount grid

bombCounts :: Grid -> Grid
bombCounts grid = mapWithKey (update' grid) grid

grid :: Grid
grid = mkGrid 5 mixed
