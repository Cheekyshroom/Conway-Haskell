import Data.List (elemIndices)
import System.Environment (getArgs)
import System.IO

data TileState = On | Off deriving (Eq, Show)
data Board = Board [[TileState]] deriving (Show)
data Game = Game Int Board | BlankGame deriving (Show)

defaultWidth = 10
defaultHeight = 10
defaultBoard = 
   Board (replicate defaultHeight (replicate defaultWidth Off))

determineState :: Int -> TileState -> TileState
determineState 2 On = On
determineState 3 _ = On
determineState n _ = Off

attachIndices [] _ = []
attachIndices lists i =
   (map (\e -> (e,i)) (head lists)) : attachIndices (tail lists) (i + 1)

getOnIndices :: Board -> [(Int, Int)]
getOnIndices (Board tiles) =
   concat (attachIndices (map (\l -> (elemIndices On l)) tiles) 0)

getClose :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
getClose indices x y =
   filter (\(i, j) -> ((abs (x-i))<=1) && ((abs (y-j))<=1))
      indices
   
countNeighbors indices x y =
   length (getClose indices x y)

indMap [] _ _ = []
indMap (head:rest) fn i =
   (fn head i) : (indMap rest fn (i+1))

nextState :: Board -> Board
nextState b@(Board tiles) =
   Board (indMap tiles 
            (\list y -> indMap list 
               (\e x -> 
                  determineState
                     (if e == On
                      then (countNeighbors indices x y)-1
                      else (countNeighbors indices x y)) e) 0) 0)
   where
      indices = getOnIndices b

nextIteration :: Game -> Game
nextIteration BlankGame =
   Game 0 defaultBoard
nextIteration (Game iteration state) =
   Game (iteration+1) (nextState state)

futureStates game =
   future : (futureStates future)
   where
      future = nextIteration game

stringToGame str =
   Game 0
      (Board (map (\line -> 
         (map (\c -> if c == '.' then Off else On) line))
            (lines str)))

readUntilBlank :: IO String
readUntilBlank = do
   line <- getLine
   if null line
   then return []
   else do
      next <- readUntilBlank
      return (line ++ ['\n'] ++ next)

getGame :: IO Game
getGame = do
   line <- readUntilBlank
   return (stringToGame line)

putTiles :: [TileState] -> IO ()
putTiles [] = do
   putChar '\n'
   return ()
putTiles (head:rest) = do
   putChar (if head == On then '#' else '.')
   putTiles rest

putBoard :: [[TileState]] -> IO ()
putBoard [] = do
   return ()
putBoard (head:rest) = do
   putTiles head
   putBoard rest

displayGame :: Game -> IO ()
displayGame g@(Game i b@(Board tiles)) = do
   putStrLn ("Turn: " ++ (show i))
   putBoard (tiles)

runGame :: Game -> IO ()
runGame game = do
   displayGame game
   line <- getLine
   if line == "quit"
   then return ()
   else
      runGame next
   where next = nextIteration game

main :: IO ()
main = do
   args <- getArgs
   if (length args) < 1
   then do
      putStrLn "Enter a game"
      game <- getGame
      runGame game
      return ()
   else do
      handle <- openFile (head args) ReadMode
      contents <- hGetContents handle
      runGame (stringToGame contents)
      hClose handle
      return ()
