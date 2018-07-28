module Minesweeper where

import qualified Data.Map.Strict as M
import Data.List hiding (group)

type Pos = (Int, Int)
type Count = Int
type Size = Int

data Grid = Grid
  { gridSize :: Size
  , container :: M.Map Pos Square
  }

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
disp :: Grid -> IO ()
disp = putStrLn . showRows

showRow :: [Square] -> String
showRow = intercalate " | " . map show

showRows :: Grid -> String
showRows grid = unlines . map showRow . group size $ sqrs
  where
    sqrs = squares grid
    pss = positions grid
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
    isBomb (BombC) = True
    isBomb _        = False

update :: Int -> Square -> Square
update n (EmptyC) = EmptyO n
update _ BombC    = BombO
update _ x        = x

update' :: Grid -> (Pos -> Square -> Square)
update' grid = update . bombCount grid

bombCounts :: Grid -> Grid
bombCounts grid = mapWithKey (update' grid) grid

grid :: Grid
grid = mkGrid 5 mixed


-- Playing the game

-- Open a grid
move :: Grid -> Pos -> Maybe Grid
move grid pos = do
  sqr <- value grid pos
  openBomb sqr
  let count = bombCount grid pos
  return $ grid { container = M.adjust (update count) pos (container grid) }

openBomb :: Square -> Maybe Square
openBomb BombC = Nothing
openBomb x     = Just x
