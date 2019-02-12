module PlayGame (playGameMain) where

import TTTGame
import Inf2d1
import Data.List (sortBy)
import Control.Exception


playGameMain = do
  putStrLn "Would you like a game of:"
  putStrLn "1. Tic-Tac-Toe "
  putStrLn "2. Wild Tic-Tac-Toe"
  getInt >>= parseInput

--parseInput :: Int -> IO ()
parseInput int 
               | int==1 = getMethod >>= (playTicTacToe initGame humanPlayer)
			   | int==2 = getMethod >>= (playWildTicTacToe initGame humanPlayer)
               | otherwise =  putStrLn "That is not a valid option."


				   
				   

				   
getMethod :: IO (Int,Int)
getMethod = do
  putStrLn "Which game search method do you want to use?"
  putStrLn "1. Minimax"
  putStrLn "2. Alphabeta"
  getInt >>= (\i -> case i of
                      1 -> return (1,0)
                      2 -> return (2,0))

getInt :: IO Int
getInt = getLine >>= (\str -> catch ((readIO str):: IO Int)
                              (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException) ++ ".\nPlease enter a valid Int value:")
                                        getInt))
-- the main function that plays Tic Tac Toe		
playTicTacToe :: Game -> Player -> (Int,Int) ->  IO ()
playTicTacToe game player (x,y) | terminal game = (drawGrid game) >> endTicTacToe game
                           | player == humanPlayer = (drawGrid game) >> getMove game >>= (\mv -> playTicTacToe (playMove game humanPlayer mv) (switch humanPlayer) (x,y))
                           | otherwise = if x == 1 
						                     then playTicTacToe (compMove minimax game) humanPlayer (x,y)
											 else playTicTacToe (compMove alphabeta game) humanPlayer (x,y)

-- the main function that plays Wild Tic Tac Toe											 
playWildTicTacToe :: Game -> Player -> (Int,Int) ->  IO ()
playWildTicTacToe game player (x,y) | terminal game = (drawGrid game) >> endWildTicTacToe game player
                           | player == humanPlayer = (drawGrid game) >> getMoveWild game >>= (\mv -> playWildTicTacToe (playMoveWild game humanPlayer mv) (switch humanPlayer) (x,y))
                           | otherwise = if x == 1 
						                     then playWildTicTacToe (compMoveWild minimaxWild game) humanPlayer (x,y)
											 else playWildTicTacToe (compMoveWild alphabetaWild game) humanPlayer (x,y)

											 
-- decides who won the game of Tic Tac Toe once a terminal state is reached.													 
endTicTacToe :: Game -> IO ()
endTicTacToe game | checkWin game humanPlayer  = putStrLn "You have won."
             | checkWin game (switch humanPlayer) = putStrLn "You have lost ... although this presumably means that your game player was quite good, so in a wider context you have actually won. Well done!"
             | otherwise = putStrLn "Draw"
			 
-- decides who won the game of Wild Tic Tac Toe once a terminal state is reached.			 
endWildTicTacToe :: Game -> Int -> IO ()
endWildTicTacToe game player| (checkWin game 0 || checkWin game 1) && player==0  = putStrLn "You have won."
             | (checkWin game 0 || checkWin game 1) && player==1 = putStrLn "You have lost ... although this presumably means that your game player was quite good, so in a wider context you have actually won. Well done!"
             | otherwise = putStrLn "Draw"

-- chooses the best Move for the computer to take in a  game of Tic Tac Toe			 
compMove::(Game->Player->Int)->Game->Game
compMove fn game = go (moves game compPlayer) (2) []
   where go availMoves score bestMove = 
          if (null availMoves)
             then bestMove
                else if (((fn (head availMoves) humanPlayer)) < score)
                   then go (tail availMoves) (fn (head availMoves) humanPlayer) (head availMoves)
                   else go (tail availMoves) score bestMove			 

-- chooses the best Move (that is cell and movetype) for the computer to take in a  game of Wild Tic Tac Toe			 				   
compMoveWild::(Game->Player->Int)->Game->Game
compMoveWild fn game = go (movesWild game compPlayer) (2) []
   where go availMoves score bestMove = 
          if (null availMoves)
             then bestMove
                else if (((fn (head availMoves) humanPlayer)) < score)
                   then go (tail availMoves) (fn (head availMoves) humanPlayer) (head availMoves)
                   else go (tail availMoves) score bestMove

--reads the cell which the human player choose as next move from the input in a  game of Tic Tac Toe	
getMove game = do
  putStrLn "Enter (row,column): "
  getPair >>= (\cell -> if (isMoveValid game humanPlayer cell)
                        then return (cell)
                        else putStrLn "Invalid move. Please enter a valid move." >> getMove game)    

--reads the cell and move type which the human player choose as next move from the input in a  game of Wild Tic Tac Toe							
getMoveWild game = do
  putStrLn "Which sign do you want to move? (x =1, o = 0)"  
  moveType<-getInt
  putStrLn "Enter (row,column): "
  getPair >>= (\cell -> if (isMoveValid game humanPlayer cell)
                        then return (cell,moveType)
                        else putStrLn "Invalid move. Please enter a valid move." >> getMoveWild game) 						
    
getPair :: IO (Int,Int)
getPair = getLine >>= (\pair -> catch((readIO pair) :: IO (Int,Int))
                                (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException) ++ ".\nPlease enter a valid (row,column):")
                                          getPair))
                              