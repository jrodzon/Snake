{-|
Module      : SnakeGameState
Description : Module is responsible for saving current game state. It also cover transforming current state into a picture.
Copyright   : Copyright (c) 2025, Jan Rodzoń
License     : MIT
Maintainer  : rodzonjan@wp.pl
Stability   : experimental
Portability : portable
The most important function here is render. It transforms current game state into a picture which is then printed on the screen. There also is initalState function, which set starting game state.
-}

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
import System.Random

-- | Datatype alias to better represent position on map.
type Position = (Int, Int)

-- | Data representing current move direction of the snake.
data MoveDirection = Upp | Downn | Leftt | Rightt deriving (Show, Eq) -- double t becouse of Prelude.Left and Prelude.Right
    -- up and down becouse of gloss library.

-- | Data describing the state of the snake game. 
data SnakeGame = Game
  { snake :: [Position]   -- ^ List of points representing snake location.
  , foodPosition :: Position      -- ^ Point of the food on the map. 
  , moveDirection :: MoveDirection  -- ^ Current direction of the snake.
  , generator :: StdGen -- ^ Current random generator used to generate pseudo random.
  , fpsCounter :: Int -- ^ FPS counter, it is used to make game faster.
  } deriving Show

-- | Initialize the game with this game state.
initialState :: StdGen  -- ^ New generator used to generate food position.
                -> SnakeGame   -- ^ Initial state of the game.
initialState gen = Game
  { snake = [(0,size), (0,0)]
  , foodPosition = (size, (-3)*size)
  , moveDirection = Upp
  , generator = gen
  , fpsCounter = 0
  }

-- | Window and map parameters.
width, height, offset :: Int
width = 1200
height = 800
offset = 100

-- | Size of snake, walls and food.
size :: Int
size = 20

-- | Draw a snake game state (convert it to a picture).
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [walls, foodPicture, snakePicture]
  where
    snakeColor = green
    foodColor = red

    -- The bottom and top walls.
    horizontalWall :: Int -> Picture
    horizontalWall offset =
      translate 0 (fromIntegral offset) $
        color wallColor $
          rectangleSolid (fromIntegral width) (fromIntegral size)

    -- The left and right walls.
    verticalWall :: Int -> Picture
    verticalWall offset =
        translate (fromIntegral offset) 0 $
        color wallColor $
            rectangleSolid (fromIntegral size) (fromIntegral height)

    wallColor = greyN 0.5
    walls = pictures [verticalWall (width `div` 2), verticalWall (- width `div` 2), horizontalWall (height `div` 2), horizontalWall (-height `div` 2)]

    -- Prepared picture of the food.
    foodPicture :: Picture
    foodPicture = pointOnTheMap (foodPosition game) foodColor

    -- Prepared picture of the snake.
    snakePicture :: Picture
    snakePicture = pictures $ map printSnakePoint (snake game)

    -- Print snake point.
    printSnakePoint :: Position -> Picture
    printSnakePoint p = pointOnTheMap p snakeColor

    -- Picture of the given point on the map.
    pointOnTheMap :: Position -> Color -> Picture
    pointOnTheMap (x, y) pointColor = translate (fromIntegral x) (fromIntegral y) $ color pointColor $ rectangleSolid (fromIntegral size) (fromIntegral size)
