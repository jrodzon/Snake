module SnakeMoveEngine
    ( update
    , handleKeys
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import SnakeGameState
import SnakeRandomGenerator

-- | Update the game by moving the snake and react properly to a situation.
update :: Float -> SnakeGame -> SnakeGame
update seconds = randomFoodPosition . selfBounce . wallBounce . moveSnake seconds

-- | Update the snake position using its current moveDirection.
moveSnake :: Float    -- ^ The number of seconds since last update
         -> SnakeGame -- ^ The initial game state
         -> SnakeGame -- ^ A new game state with an updated snake position
moveSnake seconds game = game { snake = s' }
  where
    -- Old snake and direction.
    s = snake game
    direction = moveDirection game

    -- Current food position.
    fp = foodPosition game

    --New head of the snake.
    newHead = getPointAfterMove direction $ head s

    -- New snake. If we have just eat something, we grow up.
    s' = if twoPositionCollision fp newHead then newHead : s else init $ newHead : s

    -- Converts point and moveDirection to new point after move.
    getPointAfterMove :: MoveDirection -> Position -> Position
    getPointAfterMove d (x, y) = case d of
        Upp -> (x, y+size)
        Downn -> (x, y-size)
        Leftt -> (x-size, y)
        Rightt -> (x+size, y)

-- | Detect a collision with snake himself.
selfBounce :: SnakeGame -> SnakeGame
selfBounce game = if selfCollision s then error "You hit yourself." else game
    where
        -- Current snake.
        s = snake game

-- | Detect a collision with one of the side walls.
wallBounce :: SnakeGame -> SnakeGame
wallBounce game = if wallCollision h then error "You hit the wall." else game
  where
    -- Current head position.
    h = head $ snake game

-- | Given position return whether a collision with walls occurred.
wallCollision :: Position -> Bool 
wallCollision (x, y) = topCollision || bottomCollision || leftCollision || rightCollision
  where
    topCollision    = y - size / 2 <= -fromIntegral height / 2 
    bottomCollision = y + size / 2 >=  fromIntegral height / 2
    leftCollision   = x - size / 2 <= -fromIntegral width / 2
    rightCollision  = x + size / 2 >=  fromIntegral width / 2

-- | Given snake return whether a collision occurred.
selfCollision :: [Position] -> Bool 
selfCollision (x:xs) = iterateOverSnake x xs
  where
    -- Iterates over tail of the snake and find for collision.
    iterateOverSnake :: Position -> [Position] -> Bool
    iterateOverSnake _ [] = False
    iterateOverSnake p (y:ys) = if twoPositionCollision p y then True else False || iterateOverSnake p ys

-- | Check if 2 positions are not too close.
twoPositionCollision :: Position -> Position -> Bool
twoPositionCollision (x1, y1) (x2, y2) = if abs (x1 - x2) < size / 2 && abs (y1 - y2) < size / 2 then True else False

-- | Respond to key events.
handleKeys :: Event -> SnakeGame -> SnakeGame

-- For an arrows keypress, set moveDirection into proper direction.
handleKeys (EventKey (SpecialKey arrow) Down _ _) game = case arrow of
    KeyUp -> if moveDirection game == Downn then game else game { moveDirection = Upp }
    KeyDown -> if moveDirection game == Upp then game else game { moveDirection = Downn }
    KeyLeft -> if moveDirection game == Rightt then game else game { moveDirection = Leftt }
    KeyRight -> if moveDirection game == Leftt then game else game { moveDirection = Rightt }

-- Do nothing for all other events.
handleKeys _ game = game