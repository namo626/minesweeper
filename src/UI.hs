{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}


module UI where

import Brick hiding (Result(..))
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Graphics.Vty as V
import Minesweeper
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)

data Input = Input
data Name  = MyButton Pos
  deriving (Show, Eq, Ord)

-- instance Ord Name where
--   (MyButton pos1) <= (MyButton pos2) = pos1 <= pos2

data Outcome = Won | Lost
  deriving (Show, Eq)

type Progress = Either Outcome Game

data Game = Game
  { _gameGrid :: Grid
  }

gameGrid :: Lens' Game Grid
gameGrid = lens _gameGrid (\game s -> game { _gameGrid = s } )

app :: App Game Input Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const colorMap
  }

moveGame :: Game -> Pos -> Game
moveGame game pos =
  over gameGrid ((flip move) pos) game


-- Drawing the grid

drawSquare :: Square -> Widget Name
drawSquare BombO      = withAttr deathAttr plain
drawSquare (EmptyO n) = withAttr openedAttr (plainN n)
drawSquare _          = withAttr closedAttr plain

drawPos :: Grid -> Pos -> Widget Name
drawPos grid pos = withBorderStyle BS.unicodeBold . clickable (MyButton pos) . drawSquare . value' grid $ pos

drawRow :: Grid -> [Pos] -> Widget Name
drawRow grid ps = hBox $ map (drawPos grid) ps

drawGrid :: Game -> Widget Name
drawGrid game = vBox $ map (drawRow gr) gps
  where
    gr  = game ^. gameGrid
    gps = game ^. gameGrid ^. gridPoss

drawUI :: Game -> [Widget Name]
drawUI game = [ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Game")
  $ drawGrid game ]

plain :: Widget Name
plain = str "  "

plainN :: Int -> Widget Name
plainN = str . show

deathAttr, closedAttr, openedAttr :: AttrName
deathAttr = "deathAttr"
closedAttr = "closedAttr"
openedAttr = "openedAttr"

colorMap :: AttrMap
colorMap = attrMap V.defAttr
  [ (closedAttr, V.blue `on` V.blue)
  , (openedAttr, V.red `on` V.red)
  , (deathAttr, V.yellow `on` V.yellow)
  ]


-- Mouse click opens a cell
continue' :: Game -> EventM Name (Next Game)
continue' g = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
    liftIO $ V.setMode output V.Mouse True

  continue g


handleEvent :: Game -> BrickEvent Name Input -> EventM Name (Next Game)
handleEvent g (MouseUp (MyButton pos) _ _)   = continue' $ moveGame g pos
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g
