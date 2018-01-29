module SnakeGame
    ( snakeGame
    ) where

import Graphics.Gloss.Interface.Pure.Game

import SnakeMoveEngine
import SnakeGameState

snakeGame :: IO ()
snakeGame = play window background fps initialState render handleKeys update

-- | Number of frames to show per second.
fps :: Int
fps = 5

window :: Display
window = InWindow "Snake" (width, height) (offset, offset)

background :: Color
background = black