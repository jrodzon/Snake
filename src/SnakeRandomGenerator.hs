module SnakeRandomGenerator
    ( randomFoodPosition
    ) where

import Data.Fixed

import SnakeGameState

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

-- | Check if 2 positions are not too close.
twoPositionCollision :: Position -> Position -> Bool
twoPositionCollision (x1, y1) (x2, y2) = if abs (x1 - x2) < size / 2 && abs (y1 - y2) < size / 2 then True else False