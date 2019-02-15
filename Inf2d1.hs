-- Inf2d Assignment 1 2018-2019
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = []

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth= (gridWidth_search * gridLength_search) - 1
-- Why did you choose this number?
-- You need to search the entire grid, hence the maximum depth is the total number of squares on the grid, which is the product of gridWidth_search and gridLength_search.
-- However, since you're already on the first grid, you don't have to search through the inital grid, hence the -1.

-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

-- note that type Branch = [(Int,Int)]
-- The Branch type synonym defines the branch of search through the grid.
next::Branch -> [Branch]
next [] =  []
next branch = [(x,y) : branch | (x,y) <- move(head branch), notElem (x,y) branch, x >= 1, x <= gridWidth_search, y >= 1, y <= gridLength_search, notElem (x,y) badNodesList]



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode
  | destination == curNode = True
  | otherwise = False


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch destination next [] exploredList = Nothing -- if branch is empty then there is no solution
breadthFirstSearch destination next branches exploredList
  | checkArrival destination currentNode = Just currentBranch -- if solution is found, return Just Branch
    -- check if the current node of the search branch has been explored or not
    -- explore the other branches
    -- and add the current node to the list of explored nodes
  | notElem currentNode exploredList = breadthFirstSearch destination next (tail branches ++ next currentBranch) (currentNode : exploredList)
  | otherwise = breadthFirstSearch destination next (tail branches) exploredList
      where currentNode = head (head branches)
            currentBranch = head branches

-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next [] exploredList = Nothing -- if branch is empty then no solution
depthFirstSearch destination next branches exploredList
  | checkArrival destination currentNode = Just currentBranch -- if solution is found, return Just Branch
  -- check if the current node of the search branch has been explored or not
  -- explore the child nodes of the branch
  -- and add the current node to the list of explored nodes
  | notElem currentNode exploredList = depthFirstSearch destination next (next currentBranch ++ tail branches) (currentNode : exploredList)
  | otherwise = depthFirstSearch destination next (tail branches) exploredList
      where currentNode = head (head branches)
            currentBranch = head branches

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch destination next [] d = Nothing -- if branch is empty then no solution (return Nothing)
depthLimitedSearch destination next branches d
  | d == 0 = Nothing
  | checkArrival destination currentNode = Just currentBranch -- if solution is found, return Just Branch
  -- if the length of the current branch is greater than or equal to d, skip that branch
  -- and move on to the next branch
  | branchLength >= d = depthLimitedSearch destination next (tail branches) d
  | otherwise = depthLimitedSearch destination next (next currentBranch ++ tail branches) d
      where currentNode = head (head branches)
            currentBranch = head branches
            branchLength = length currentBranch

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
  -- if solution is found, return a branch containing the Node
  -- d here is maxDepth?? NOT DONE
  | checkArrival destination initialNode = Just (branchify initialNode)
  | depthLimitedSearch destination next [branchify initialNode] d == Nothing = iterDeepSearch destination next initialNode (d+1)
  | otherwise = depthLimitedSearch destination next [branchify initialNode] d
      where branchify x = [x]


-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan position destination = abs(fst position - fst destination) + abs(snd position - snd destination)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

-- TODO: manhattan != heuristic?
-- FIX!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- bestFirstSearch (6,6) next (manhattan (6,6)) [[(1,2),(5,6)],[(2,3)]] []
bestFirstSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> [Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic [] exploredList = Nothing
bestFirstSearch destination next heuristic branches exploredList
  | checkArrival destination currentNode = Just currentBranch
  | notElem currentNode exploredList = bestFirstSearch destination next (manhattan destination) (sortBy (compareBranches manhattan destination) (tail branches ++ next currentBranch)) (currentNode : exploredList)
  | otherwise = bestFirstSearch destination next (manhattan destination) (sortBy (compareBranches manhattan destination) ((tail branches) ++ next currentBranch)) exploredList
      where currentNode = head (head branches)
            currentBranch = head branches

-- compare the branches based on their heuristic
compareBranches::(Node-> Node-> Int) -> Node -> Branch -> Branch -> Ordering
compareBranches heuristic destination branch1 branch2 = compare (heuristic destination (head branch1)) (heuristic destination (head branch2))

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

-- TODO manhattan != heuristic?
aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost [] exploredList = Nothing -- if there is no solution return Nothing
aStarSearch destination next heuristic cost branches exploredList
  | checkArrival destination currentNode = Just currentBranch
  | notElem currentNode exploredList = aStarSearch destination next (manhattan destination) cost (sortBy (compareBranches manhattan destination) (tail branches ++ next currentBranch)) (currentNode : exploredList)
  | otherwise = aStarSearch destination next (manhattan destination) cost (sortBy (compareBranches manhattan destination) ((tail branches) ++ next currentBranch)) exploredList
        where currentNode = head (head branches)
              currentBranch = head branches

-- compare the branches based on their heuristic + cost
compareBranchesWithCost::(Node-> Node-> Int) -> Node -> Branch -> Branch -> Ordering
compareBranchesWithCost heuristic destination branch1 branch2 = compare (cost branch1 + (heuristic destination (head branch1))) (cost branch2 + (heuristic destination (head branch2)))

-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
  | terminal game && playerOneWins  = 1
  | terminal game && playerZeroWins = -1
  | terminal game                   = 0
      where playerOneWins   = checkWin game 1
            playerZeroWins  = checkWin game 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

-- TODO: READ ON THIS
minimax:: Game->Player->Int
minimax game player
  | terminal game = eval game
  | maxPlayer player = maximum [minimax g (switch player) | g <- moves game player]
  | minPlayer player = minimum [minimax g (switch player) | g <- moves game player]


-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

-- TODO
-- ALPHABETA RANGE IS (-2, +2)
alphabeta:: Game->Player->Int
alphabeta game player
  | terminal game = eval game
  | maxPlayer player = undefined
  | minPlayer player = undefined

-- PSEUDO CODE ON THE ASSIGNMENT



-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

-- TODO
evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game =undefined



-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

-- TODO
alphabetaWild:: Game->Player->Int
alphabetaWild game player =undefined



-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

-- TODO
minimaxWild:: Game->Player->Int
minimaxWild game player =undefined



			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores

move :: Node -> Branch
move (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
