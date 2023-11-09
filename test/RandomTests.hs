import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data Player
  = O
  | B
  | X
  deriving (Eq, Ord, Show)

instance Arbitrary Player where
  arbitrary = elements [O, B, X]

type Grid = [[Player]]

--Función turn
turn :: Grid -> Player
turn g =
  if os <= xs
    then O
    else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

--Propiedad: La función turn debe devolver
--O o X como resultado.
prop_turn :: Grid -> Bool
prop_turn g = resultValido (turn g)
  where
    resultValido player = player == O || player == X

main :: IO ()
main = do
  quickCheck prop_turn
