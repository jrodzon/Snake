module UnitTests (unitTests) where

import Test.HUnit
import SnakeMoveEngine
import SnakeGameState

testGetOppositeMoveDirectionRight :: Test
testGetOppositeMoveDirectionRight = 
  TestCase $ assertEqual
    "Function getOppositeMoveDirection does not work properly."
    Leftt
    (getOppositeMoveDirection Rightt)

testGetOppositeMoveDirectionLeft :: Test
testGetOppositeMoveDirectionLeft = 
  TestCase $ assertEqual
    "Function getOppositeMoveDirection does not work properly."
    Rightt
    (getOppositeMoveDirection Leftt)
  
testGetOppositeMoveDirectionUp :: Test
testGetOppositeMoveDirectionUp = 
  TestCase $ assertEqual
    "Function getOppositeMoveDirection does not work properly."
    Upp
    (getOppositeMoveDirection Downn)

testGetOppositeMoveDirectionDown :: Test
testGetOppositeMoveDirectionDown = 
  TestCase $ assertEqual
    "Function getOppositeMoveDirection does not work properly."
    Downn
    (getOppositeMoveDirection Upp)

testConvertPositionsToMoveDirectionRight :: Test
testConvertPositionsToMoveDirectionRight = 
  TestCase $ assertEqual
    "Function convertPositionsToMoveDirection does not work properly."
    Rightt
    (convertPositionsToMoveDirection (10, 10) (20, 10))

testConvertPositionsToMoveDirectionLeft :: Test
testConvertPositionsToMoveDirectionLeft = 
  TestCase $ assertEqual
    "Function convertPositionsToMoveDirection does not work properly."
    Leftt
    (convertPositionsToMoveDirection (10, 10) (0, 10))

testConvertPositionsToMoveDirectionUp :: Test
testConvertPositionsToMoveDirectionUp = 
  TestCase $ assertEqual
    "Function convertPositionsToMoveDirection does not work properly."
    Upp
    (convertPositionsToMoveDirection (10, 10) (10, 20))

testConvertPositionsToMoveDirectionDown :: Test
testConvertPositionsToMoveDirectionDown = 
  TestCase $ assertEqual
    "Function convertPositionsToMoveDirection does not work properly."
    Downn
    (convertPositionsToMoveDirection (10, 10) (10, 0))

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