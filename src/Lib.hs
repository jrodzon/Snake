module Lib
    ( someFunc
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed 

someFunc :: IO ()
someFunc = play window background fps initialState render handleKeys update


-- | Update the game by moving the snake and react properly to a situation.
update :: Float -> SnakeGame -> SnakeGame
update seconds = randomFoodPosition . selfBounce . wallBounce . moveSnake seconds

-- | Number of frames to show per second.
fps :: Int
fps = 5

width, height, offset :: Int
width = 400
height = 400
offset = 100

window :: Display
window = InWindow "Snake" (width, height) (offset, offset)

background :: Color
background = black

-- | Size of snake, walls and food.
size :: Float
size = 10

type Position = (Float, Float)

-- | Data representing current move direction of the snake.
data MoveDirection = Upp | Downn | Leftt | Rightt deriving (Show, Eq) -- double t becouse of Prelude.Left and Prelude.Right
    -- up and down becouse of gloss library.

-- | Data describing the state of the snake game. 
data SnakeGame = Game
  { snake :: [Position]   -- ^ List of points representing snake location.
  , foodPosition :: Position      -- ^ Point of the food on the map. 
  , moveDirection :: MoveDirection  -- ^ Current direction of the snake.
  , seed :: Float -- ^ Current seed used to generate pseudo random.
  } deriving Show

-- | Initialize the game with this game state.
initialState :: SnakeGame   -- ^ Initial state of the game.
initialState = Game
  { snake = [(0,size), (0,0)]
  , foodPosition = (size, (-3)*size)
  , moveDirection = Upp
  , seed = 123456789
  }

-- | Draw a snake game state (convert it to a picture).
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [walls, foodPicture, snakePicture]
  where
    snakeColor = greyN 0.5

    -- The bottom and top walls.
    horizontalWall :: Float -> Picture
    horizontalWall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 400 size

    -- The left and right walls.
    verticalWall :: Float -> Picture
    verticalWall offset =
        translate offset 0 $
        color wallColor $
            rectangleSolid size 400

    wallColor = greyN 0.5
    walls = pictures [verticalWall 200, verticalWall (-200), horizontalWall 200, horizontalWall (-200)]

    -- Prepared picture of the food.
    foodPicture :: Picture
    foodPicture = pointOnTheMap $ foodPosition game

    -- Prepared picture of the snake.
    snakePicture :: Picture
    snakePicture = pictures $ map pointOnTheMap $ snake game

    -- Picture of the given point on the map.
    pointOnTheMap :: Position -> Picture
    pointOnTheMap (x, y) = translate x y $ color snakeColor $ rectangleSolid size size


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

-- | Modify food position if food is consumped.
randomFoodPosition :: SnakeGame -> SnakeGame
randomFoodPosition game = if twoPositionCollision fp h then game' { foodPosition = fp'} else game
    where
        -- Current food position.
        fp = foodPosition game

        -- Current head of the snake.
        h = head $ snake game

        -- New food position if needed
        fp' = setNewRandomFoodPosition (fp'x, fp'y)

        game' = newSeed game
        
        fp'x :: Float
        fp'y :: Float

        fp'x = getRandom game' (-fromIntegral width / 2 + size, fromIntegral width / 2 - size)
        fp'y = getRandom game' (-fromIntegral height / 2 + size, fromIntegral height / 2 - size)

        setNewRandomFoodPosition :: Position -> Position
        setNewRandomFoodPosition (x, y) = (fromIntegral $ ceiling x,fromIntegral $ ceiling y)

-- | Generate new seed used to create new random.
newSeed :: SnakeGame -> SnakeGame
newSeed game = game { seed = (mod') (1103515245 * oldSeed + 12345) (2^32) }
            where
                oldSeed = seed game

getRandom :: SnakeGame -> Position -> Float
getRandom game (min, max) = (mod') newSeed (max - min) + min
    where
    newSeed = seed game