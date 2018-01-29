module QuickCheckTests (quickCheckTests) where

import Test.QuickCheck

import SnakeGameState
import SnakeMoveEngine
import SnakeRandomGenerator

instance Arbitrary MoveDirection where
   arbitrary = do
    x <- arbitrary
    return $ [Leftt, Rightt, Upp, Downn] !! (x `mod` 4)

-- List which will be used to generate possible moves.
specialList :: [Int]
specialList = [-width `quot` 2 + size, -width `quot` 2 .. width `quot` 2 - 2 * size]

prop_ConvertPositionsToMoveDirection :: Position -> Bool
prop_ConvertPositionsToMoveDirection (x, y) = checkHorizontal (x * size, y * size) && checkVertical (x * size, y * size)

checkHorizontal :: Position -> Bool
checkHorizontal (x, y) = convertPositionsToMoveDirection (x, y) (x, y + size) == getOppositeMoveDirection (convertPositionsToMoveDirection (x, y + size) (x, y))

checkVertical :: Position -> Bool
checkVertical (x, y) = convertPositionsToMoveDirection (x, y) (x + size, y) == getOppositeMoveDirection (convertPositionsToMoveDirection (x + size, y) (x, y))

prop_GetOppositeMoveDirection :: MoveDirection -> Bool
prop_GetOppositeMoveDirection x = getOppositeMoveDirection (getOppositeMoveDirection x) == x

quickCheckTests = do
    quickCheck prop_GetOppositeMoveDirection
    quickCheck prop_ConvertPositionsToMoveDirection