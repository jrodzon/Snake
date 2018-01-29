module UnitTests (unitTests) where



tests :: Test
tests = TestList [
  TestLabel "testSize" testSize,
  TestLabel "testPeek" testPeek,
  TestLabel "testToList" testToList,
  TestLabel "testPop" testPop ]


unitTests = do
    runTestTT tests