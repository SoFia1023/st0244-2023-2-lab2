import Test.HUnit

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player
  = O
  | B
  | X
  deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

--Función valid
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

--Función full
full :: Grid -> Bool
full = notElem B . concat

--Función wins
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

--Función empty
empty :: Grid
empty = replicate size (replicate size B)

-- Pruebas para la función valid
testValid :: Test
testValid = TestList
    [ TestCase $ assertEqual "Movimiento válido dentro de los límites" True (valid empty 4)
    , TestCase $ assertEqual "Movimiento no válido fuera de los límites" False (valid empty (-1))
    , TestCase $ assertEqual "Movimiento no válido en una celda que no está vacía" False (valid [[X, O, B], [B, O, B], [X, B, X]] 6)
    ]

-- Pruebas para la función full
testFull :: Test
testFull = TestList
    [ TestCase $ assertEqual "Tablero vacío" False (full empty)
    , TestCase $ assertEqual "Tablero lleno" True (full [[X, O, X], [X, O, O], [O, X, X]])
    ]

-- Pruebas para la función wins
testWins :: Test
testWins = TestList
    [ TestCase $ assertEqual "Jugador O gana" True (wins O [[O, X, X], [O, B, O], [O, X, B]])
    , TestCase $ assertEqual "Jugador X gana" True (wins X [[O, X, B], [X, X, X], [O, B, X]])
    , TestCase $ assertEqual "Ningún jugador gana" False (wins O [[O, X, X], [X, O, X], [O, O, B])
    ]

main :: IO Counts
main = do
    runTestTT $ TestList [testValid, testFull, testWins]
    
