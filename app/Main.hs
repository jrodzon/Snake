{-|
Module      : Main
Description : Main module. It runs the application.
Copyright   : Copyright (c) 2017, Jakub Kołoczek & Jan Rodzoń
License     : MIT
Maintainer  : rodzonjan@wp.pl
Stability   : experimental
Portability : portable
-}

module Main where

import SnakeGame

-- | Run the application.
main :: IO ()
main = snakeGame
