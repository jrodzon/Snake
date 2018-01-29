import QuickCheckTests
import UnitTests

main :: IO ()
main = do
  quickCheckTests
  unitTests
  return ()
