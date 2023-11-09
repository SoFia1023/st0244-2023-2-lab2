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

--Tablero vacío size x size
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

--Le asigna el siguiente turno al jugador, teniendo
--en cuenta la cantidad de "X" y "O" en el tablero.
turn :: Grid -> Player
turn g =
  if os <= xs
    then O
    else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g


--Determina si un jugador a ganado analizando 
--filas,columnas y diagonales.
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

--Permite mostrar por consola el tablero de forma legible
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

--Si el movimiento del jugador es válido, permite visualizar el 
--tablero que se tenía con la nueva actualización.
move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i
    then [chop size (xs ++ [p] ++ ys)]
    else []
  where
    (xs, B:ys) = splitAt i (concat g)

--Divide una lista en sublistas.
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

--Adquiere un valor de tipo numérico ingresado por el
--usuario y comprueba que sea un válido.
getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

--Limpia,muestra el tablero y continúa el juego.
run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

--Se hace uso de la recursividad.Determina el estado del juego,si
--un jugador ganó,si quedaron en empate o si se debe de seguir
--con el juego.
run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a =
  Node a [Tree a]
  deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

--Retorna una lista con los posibles movimientos de un jugador.
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

--Toma el estado actual del tablero y busca la mejor
--estrategia para el jugador actual.
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree


--Pregunta si el humano desea jugar primero; devuelve
--el jugador seleccionado.
orderofplayers :: IO Player
orderofplayers = do
  putStr "Quiere jugar primero (sí/no)?: "
  choice <- getLine
  if choice == "sí"
    then return O
    else if choice == "no"
      then return X
      else do
        putStrLn "Opción no válida. Por favor, ingrese 'sí' o 'no'."
        orderofplayers

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  firstPlayer <- orderofplayers
  play empty firstPlayer

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play' g p
      [g'] -> play g' (next p)
  | p == X = do
    putStrLn "Player X is thinking... "
    (play $! bestmove g p) (next p)

