module SnakeGame
    ( snakeGame
    ) where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

import SnakeMoveEngine
import SnakeGameState

snakeGame :: IO ()
snakeGame = do
    g <- newStdGen
    play window background fps (initialState g) render handleKeys update

window :: Display
window = InWindow "Snake" (width, height) (offset, offset)

background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60