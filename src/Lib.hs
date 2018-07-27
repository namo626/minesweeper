-- module Lib where

-- import Data.List hiding (group)


-- data Square = Empty Status | Bomb Status
--   deriving (Eq, Show)

-- data Status = Opened | Closed
--   deriving (Eq, Show)

-- type Coor = (Int, Int)

-- type Grid a = [[a]]

-- size = 3

-- replicateGrid :: Square -> Grid Square
-- replicateGrid s = replicate size (replicate size s)

-- empty :: Grid Square
-- empty = replicateGrid $ Empty Closed

-- indices :: [Coor]
-- indices = [ (i, j) | i <- range, j <- range ]
--   where
--     range = [0 .. size-1]

-- allBombs :: Grid Square
-- allBombs = replicateGrid (Bomb Closed)

-- mixed :: Grid Square
-- mixed = replicate size (replicate (size-2) (Empty Closed)  ++ replicate 2 (Bomb Closed))

-- won :: Grid Square
-- won = replicateGrid $ Empty Opened

-- -- Player wins when all empty squares are opened == no closed empty squares
-- wins :: Grid (Int, Square) -> Bool
-- wins = all (not . isEmpty) . concat
--   where
--     isEmpty (Empty Closed _) = True
--     isEmpty _ = False

-- -- Loses when a bomb has been opened
-- loses :: Grid (Int, Square) -> Bool
-- loses = any isBomb . concat . map snd
--   where
--     isBomb = (== Bomb Closed)

-- adjacents :: Coor -> [Coor]
-- adjacents (i, j) = [(x, y) | x <- map ($i) range, y <- map ($j) range, (x, y) /= (i, j)]
--   where
--     range = [dec, (+0), inc]
--     dec a = a - 1
--     inc a = a + 1

-- pruneAdjs :: [Coor] -> [Coor]
-- pruneAdjs = filter inLimits
--   where
--     inLimits (a, b) = a >= 0 && a < size && b >= 0 && b < size

-- adjacents' :: Coor -> [Coor]
-- adjacents' = pruneAdjs . adjacents

-- -- | Count the number of adjacent bombs of a coordinate
-- countBombs :: Grid -> Coor -> Int
-- countBombs grid coor = length . filter (== Bomb Closed) $ values
--   where
--     coors = adjacents' coor
--     values = [ grid !! i !! j | (i, j) <- coors ]

-- totalBombs :: Grid -> Int
-- totalBombs grid = sum . map (countBombs grid) $ indices

-- -- | Fill the grid with adjacent bomb counts
-- fillCounts :: Grid -> Grid
-- fillCounts grid = zipWith f (group $ map (countBombs grid) indices) grid
--   where
--     f  = zipWith g
--     g n (Empty _ _) = Empty Closed n
--     g _ s = s


-- group :: [a] -> [[a]]
-- group xs = take size xs : group (drop size xs)


-- {- Displaying a grid -}
-- display :: Grid -> IO ()
-- display = mapM_ (putStrLn . show)


-- {- Playing the game -}

-- -- | Open a square to reveal an empty space or a bomb
-- open :: Grid -> Coor -> Grid
-- open grid (i, j) = replace grid row' i
--   where
--     row = grid !! i
--     row' = case row !! j of
--       Empty Closed bombs -> replace row (Empty Opened bombs) j
--       Bomb Closed -> replace row (Bomb Opened) j

-- replace :: [a] -> a -> Int -> [a]
-- replace zs z i = xs ++ [z] ++ ys
--   where
--     (xs, a:ys) = splitAt i zs
