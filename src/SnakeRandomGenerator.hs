{-|
Module      : SnakeRandomGenerator
Description : Module is responsible for finding new place for food.
Copyright   : Copyright (c) 2017, Jakub Kołoczek & Jan Rodzoń
License     : MIT
Maintainer  : rodzonjan@wp.pl
Stability   : experimental
Portability : portable
The most important function here is randomFoodPositions. It generate list of empty positions of map and then choose random position from generated list.
-}

module SnakeRandomGenerator ( randomFoodPosition ) where

import System.Random

import SnakeGameState

-- | Modify food position if food is consumped.
randomFoodPosition :: SnakeGame -- ^ State of the game before checking food position.
                    -> SnakeGame    -- ^ State of the game after checking and if necessary changing food position.
randomFoodPosition game = if fp == h then game { foodPosition = fp', generator = snd randomPair} else game
    where
        -- Current food position.
        fp = foodPosition game

        -- Current head of the snake.
        h = head $ snake game

        -- List of possible positions for new food.
        emptyPositionsList = getEmptyPositionsList game

        -- New food position if needed.
        fp' = getNewFoodPosition emptyPositionsList (fst randomPair)

        -- Pair of random number and new random generator returned by the funcion.
        randomPair = randomR (0, (length emptyPositionsList) - 1) (generator game)

-- Return random position. Got list of possible positions and random number in scope.
getNewFoodPosition :: [Position] -> Int -> Position
getNewFoodPosition [] _ = error "Your snake is too big for this map. You are a vanquisher :)"
getNewFoodPosition list i = if length list - 1 < i then error "Fatal error, length of given list is shorter then index to read." else list !! i

-- Create a list of empty positions on the map.
getEmptyPositionsList :: SnakeGame -> [Position]
getEmptyPositionsList game = [t | t <- getMapPositionsList, not $ elem t (snake game)]

-- Return list of positions on the map.
getMapPositionsList :: [Position]
getMapPositionsList = [(x, y) | x <- getRowXList, y <- getColumnYList]

-- Return list of x coordinates of full row of the map.
getRowXList :: [Int]
getRowXList = [-width `quot` 2 + size, -width `quot` 2 + 2 * size .. width `quot` 2 - size]

-- Return list of y coordinates of full column of the map.
getColumnYList :: [Int]
getColumnYList = [-height `quot` 2 + size, -height `quot` 2 + 2 * size .. height `quot` 2 - size]