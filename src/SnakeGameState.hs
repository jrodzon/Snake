module SnakeGameState
    ( Position
    , MoveDirection (..)
    , SnakeGame (..)
    , width
    , height
    , offset
    , size
    , initialState
    , render
    ) where

import Graphics.Gloss

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


width, height, offset :: Int
width = 400
height = 400
offset = 100

-- | Size of snake, walls and food.
size :: Float
size = 10

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