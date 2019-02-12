module TTTGame where

import Data.List (sortBy)
import Debug.Trace

-- the game board is represented as a list of Int types, typically of length 9
type Game = [Int]

--the player is represented as an Int type: Max player is repesented as 1 and 
--min player is 0. -1 represents an unplayed cell
type Player = Int
-- the Cell type represents any one of the 9 points on the board, given by its (row, column) pair
type Cell = (Int,Int)
-- The Line type represents any of the lines on the game board: rows, columns and diagonals
type Line = [Int]

-- Intializes the game board; -1 represents an unplayed cell
initGame::Game
initGame = [-1,-1,-1,-1,-1,-1,-1,-1,-1]

-- maxPlayer function checks if the given player is max, and returns a Boolean
maxPlayer::Player->Bool
maxPlayer 0 = False
maxPlayer 1 = True

-- minPlayer function checks if the given player is min, and returns a Boolean
minPlayer::Player->Bool
minPlayer 0 = True
minPlayer 1 = False

humanPlayer :: Player 
humanPlayer = 1

compPlayer :: Player 
compPlayer = 0

-- switch function alternates between players
switch::Player->Player
switch p 
    | (p == humanPlayer) = 0
    | otherwise = humanPlayer
    
    
-- getCellIndex maps the (row,column) coordinate of a cell to the corresponding 
-- index on 9-element list representing the game type
getCellIndex::Cell->Int
getCellIndex cell = (3 * ((fst cell) -1)) + ((snd cell) -1)

-- getLines returns a list of all lines on the game board with the position of max (x) and mins (o) on the board
getLines::Game->[Line]
getLines g = {--rows--}((g !! 0) : (g !! 1) : (g !! 2):[]):[] ++ ((g !! 3) : (g !! 4) : (g !! 5):[]):[] ++ ((g !! 6) : (g !! 7) : (g !! 8):[]):[] ++ {--cols--} ((g !! 0) : (g !! 3) : (g !! 6):[]):[] ++ ((g !! 1) : (g !! 4) : (g !! 7):[]):[] ++ ((g !! 2) : (g !! 5) : (g !! 8):[]):[] ++{--diags--}((g !! 0):[]):[] ++ ((g !! 1) : (g !! 3) :[]):[] ++ ((g !! 2) : (g !! 4) : (g !! 6):[]):[] ++ ((g !! 5) : (g !! 7) :[]):[] ++ ((g !! 8):[]):[] ++ ((g !! 2):[]):[] ++ ((g !! 1) : (g !! 5) :[]):[] ++ ((g !! 0) : (g !! 4) : (g !! 8):[]):[] ++ ((g !! 3) : (g !! 7) :[]):[] ++ ((g !! 6):[]):[]

		  
-- terminal function checks if the game argument is in a terminal state: 
-- either a win state for a player or a state with no sucessors 
-- Note that you have to interpret this function  differently for Tic Tac Toe and Wild Tic Tac Toe:
-- in Tic Tac Toe the if statement is equal to: if(checkWin g humanPlayer) || (checkWin g (switch(humanPlayer))) (that is 0 denotes the computer player and 1 denotes the human player)
-- in Wild Tic Tac Toe, the 0 and 1 symbolise the different move types ((0 =o, 1= x)
terminal::Game-> Bool
terminal g = 
    if(checkWin g 0) || (checkWin g 1)
       then         True
       else if (not((-1) `elem` g))
          then True
          else False



-- availableCells function returns a list of Int indices for all empty cells on the game board
availableCells::Game->[Int]
availableCells g = go 0 []
    where go id cellList = 
           if (id > ((length g) - 1))
              then reverse cellList
              else if ( g !! id) == (-1)
                 then go (id + 1) (id:cellList)
                 else go (id + 1) cellList

-- isMoveValid checks if a move made in a given game state is a valid one for a given player
isMoveValid::Game->Player->Cell->Bool
isMoveValid g p cell 
    | ((g !! (getCellIndex cell) ) /= (-1))  = False        
    | otherwise = True


-- playMove makes a move to a cell and returns the new game state. This functions is called for human player moves in the Tic Tac Toe game
playMove::Game->Player->Cell->Game
playMove g p cell = go (getCellIndex cell) ((length g)-1) []
   where go cellId id newGameState = 
          if (id < 0)
             then newGameState
             else if (cellId == id)
                then go cellId (id - 1) (p:newGameState)
                else go cellId (id - 1) ((g !! id):newGameState)	

-- playWildMove makes a move of type mvtype (x or o) to a cell and returns the new game state. This functions is called for human player moves in the Wild Tic Tac Toe game
playMoveWild::Game->Player->(Cell,Int)->Game
playMoveWild g p (cell,mvtype) = 
  
  go (getCellIndex cell) ((length g)-1) []
   where go cellId id newGameState = 
          if (id < 0)
             then newGameState
             else if (cellId == id)
                then go cellId (id - 1) (mvtype:newGameState)
                else go cellId (id - 1) ((g !! id):newGameState)

				
-- playMoveById is similar to playMove, but uses the index of the board cell. It is called by the computer when it makes its moves
playMoveById::Game->Player->Int->Game
playMoveById g p cellId = go cellId ((length g)-1) []
   where go cellId id newGameState = 
          if (id < 0)
             then newGameState
             else if (cellId == id)
                then go cellId (id - 1) (p:newGameState)
                else go cellId (id - 1) ((g !! id):newGameState)       

				
-- playWildMoveById is similar to playWildMove, but uses the index of the board cell. It is called by the computer when it makes its moves
playWildMoveById::Game->Player->Int-> Int->Game
playWildMoveById g p cellId mvtype= go cellId ((length g)-1) []
   where go cellId id newGameState = 
          if (id < 0)
             then newGameState
             else if (cellId == id)
                then go cellId (id - 1) (mvtype:newGameState)
                else go cellId (id - 1) ((g !! id):newGameState)       

				

-- moves function returns a list of possible moves/successor states that a player can make given a game state for Tic Tac Toe.  
moves::Game->Player->[Game]
moves g p = go (reverse(availableCells g)) []
   where go::[Int]->[Game]->[Game]
         go availCells availMoves = 
            if (null availCells)
               then availMoves
               else go (tail availCells) ((playMoveById g p (head availCells)):availMoves )
				
-- movesWild function returns a list of possible moves/successor states that a player can make given a game state for Wild Tic Tac Toe. 
movesWild::Game->Player->[Game]
movesWild g p = go (reverse(availableCells g)) []
   where go::[Int]->[Game]->[Game]
         go availCells availMoves = 
            if (null availCells)
               then availMoves
               else go (tail availCells) ((playWildMoveById g p (head availCells) 0):(playWildMoveById g p (head availCells) 1):availMoves )
			   

-- checkWin function checks if the game state is a win for the player or movetype argument.                         
checkWin::Game->Int->Bool
checkWin g mvType = go (getLines g) 0
   where go lines acc = 
          if (acc == 3) then True
             else if (null lines)
                then False
                else if (null (head lines)) && (length  lines == 1)
                   then False
                   else if (null (head lines))
                      then go (tail lines) 0
                      else if (head(head lines) == mvType)
                         then go (tail(head lines) : (tail lines)) (acc + 1)
                         else go (tail(head lines) : (tail lines)) 0

-- | drawGrid draws and displays the game board on the screen
drawGrid::Game->IO()
drawGrid g = go 0 []
    where go id gridString  =
           if (id >= 8 )
              then putStrLn ("\n" ++ gridString ++ "\n ") ::IO ()
              else if id `elem` [0,1,2]
                 then go (id + 3) ( gridString ++ " " ++ drawPlayerMark(g !! 0) ++ " | " ++ drawPlayerMark(g !! 1) ++ " | " ++ drawPlayerMark(g !! 2) ++ "\n--- --- ---\n")
                 else if id `elem` [1,2,3]
                    then go (id + 3) (gridString ++ " " ++ drawPlayerMark(g !! 3) ++ " | " ++ drawPlayerMark(g !! 4) ++ " | " ++ drawPlayerMark(g !! 5) ++ "\n--- --- ---\n")
                    else go (id + 3) (gridString ++ " " ++ drawPlayerMark(g !! 6) ++ " | " ++ drawPlayerMark(g !! 7) ++ " | " ++ drawPlayerMark(g !! 8))

-- defines the 'x' and 'o' marks that are placed on the game board for max and min players
drawPlayerMark::Int->String
drawPlayerMark mark  
        | mark == 1  = "x"
        | mark == 0  = "o"
        | otherwise  = " "
                                         

