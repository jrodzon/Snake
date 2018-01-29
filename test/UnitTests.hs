module UnitTests (unitTests) where

import Test.HUnit
import SnakeMoveEngine
import SnakeGameState

testGetOppositeMoveDirectionRight :: Test
testGetOppositeMoveDirection = 
  TestCase $ assertEqual
    "Function getOppositeMoveDirection does not work properly."
    Leftt
    (getOppositeMoveDirection Rightt)

testGetOppositeMoveDirectionLeft :: Test
testGetOppositeMoveDirection = 
  TestCase $ assertEqual
    "Function getOppositeMoveDirection does not work properly."
    Rightt
    (getOppositeMoveDirection Leftt)
  
testGetOppositeMoveDirectionUp :: Test
testGetOppositeMoveDirection = 
  TestCase $ assertEqual
    "Function getOppositeMoveDirection does not work properly."
    Upp
    (getOppositeMoveDirection Downn)

testGetOppositeMoveDirectionDown :: Test
testGetOppositeMoveDirection = 
  TestCase $ assertEqual
    "Function getOppositeMoveDirection does not work properly."
    Downn
    (getOppositeMoveDirection Upp)

testConvertPositionsToMoveDirectionRight :: Test
testConvertPositionsToMoveDirection = 
  TestCase $ assertEqual
    "Function convertpositionsToMoveDirection does not work properly."
    Rightt
    (convertpositionsToMoveDirection (10, 10) (20, 10))

testConvertPositionsToMoveDirectionLeft :: Test
testConvertPositionsToMoveDirection = 
  TestCase $ assertEqual
    "Function convertpositionsToMoveDirection does not work properly."
    Leftt
    (convertpositionsToMoveDirection (10, 10) (0, 10))

testConvertPositionsToMoveDirectionUp :: Test
testConvertPositionsToMoveDirection = 
  TestCase $ assertEqual
    "Function convertpositionsToMoveDirection does not work properly."
    Upp
    (convertpositionsToMoveDirection (10, 10) (10, 20))

testConvertPositionsToMoveDirectionDown :: Test
testConvertPositionsToMoveDirection = 
  TestCase $ assertEqual
    "Function convertpositionsToMoveDirection does not work properly."
    Downn
    (convertpositionsToMoveDirection (10, 0) (10, 10))

tests :: Test
tests = TestList [
  TestLabel "testGetOppositeMoveDirectionRight" testGetOppositeMoveDirectionRight,
  TestLabel "testGetOppositeMoveDirectionLeft" testGetOppositeMoveDirectionLeft,
  TestLabel "testGetOppositeMoveDirectionUp" testGetOppositeMoveDirectionUp,
  TestLabel "testGetOppositeMoveDirectionDown" testGetOppositeMoveDirectionDown,
  TestLabel "testConvertPositionsToMoveDirectionRight" testConvertPositionsToMoveDirectionRight,
  TestLabel "testConvertPositionsToMoveDirectionLeft" testConvertPositionsToMoveDirectionLeft,
  TestLabel "testConvertPositionsToMoveDirectionUp" testConvertPositionsToMoveDirectionUp,
  TestLabel "testConvertPositionsToMoveDirectionDown" testConvertPositionsToMoveDirectionDown ]


unitTests = do
    runTestTT tests