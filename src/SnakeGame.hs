{-|
Module      : SnakeGame
Description : Main module responsible for plaing game.
Copyright   : Copyright (c) 2017, Jakub Kołoczek & Jan Rodzoń
License     : MIT
Maintainer  : rodzonjan@wp.pl
Stability   : experimental
Portability : portable
The most important function here is snakeGame, which generate new random generator and then run function play from Gloss library. It also has some parameters about window.
-}

module SnakeGame
    ( snakeGame
    ) where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

import SnakeMoveEngine
import SnakeGameState

-- | Function generates new random generator and then run function play from Gloss library.
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