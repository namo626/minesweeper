module UI where

import Brick
import Minesweeper

newtype Input = Input
type Name = ()

data Game = Game
  { _grid :: Grid
  , _focus :: Focus
  }

app :: App Game Input Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

move' :: Game -> Pos -> Maybe Game
move' g p = g { _grid = move p grid
              , _focus = move focus
              }

drawUI :: Game -> [Widget Name]

handleEvent :: Game -> BrickEvent Name Input -> EventM Input (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [] )) = continue $ move' g
