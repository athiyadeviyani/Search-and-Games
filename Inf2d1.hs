-- Inf2d Assignment 1 2018-2019
-- Matriculation number: s1709906
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
  | (notElem currentNode exploredList) = breadthFirstSearch destination next (tail branches ++ next currentBranch) (currentNode : exploredList)
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
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int -> Maybe Branch
depthLimitedSearch destination next [] d = Nothing -- if branch is empty then no solution (return Nothing)
depthLimitedSearch destination next branches d
  | checkArrival destination currentNode = Just currentBranch -- if solution is found, return Just Branch
  -- if the length of the current branch is greater than or equal to d, skip that branch
  -- and move on to the next branch
  | (branchLength <= d) && (notElem currentNode badNodesList) = depthLimitedSearch destination next (next currentBranch ++ tail branches) d
  | otherwise = depthLimitedSearch destination next branches d
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
  | elem initialNode badNodesList = Nothing
  | otherwise = iterDeepSearch' destination next [[initialNode]] d

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.
manhattan::Node->Node->Int
manhattan position destination = abs(fst position - fst destination) + abs(snd position - snd destination)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.
bestFirstSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> [Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic [] exploredList = Nothing
bestFirstSearch destination next heuristic branches exploredList
  | elem destination badNodesList = Nothing
  | checkArrival destination currentNode = Just currentBranch
  | notElem currentNode exploredList = bestFirstSearch destination next heuristic (next currentBranch ++ tail sortedBranches) (currentNode : exploredList)
  | otherwise = bestFirstSearch destination next heuristic (tail sortedBranches) exploredList
      where sortedBranches = sortBranches branches heuristic
            currentBranch = head sortedBranches
            currentNode = head currentBranch

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.
aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost [] exploredList = Nothing -- if there is no solution return Nothing
aStarSearch destination next heuristic cost branches exploredList
  | elem destination badNodesList = Nothing
  | checkArrival destination currentNode = Just currentBranch
  | notElem currentNode exploredList = aStarSearch destination next heuristic cost (next currentBranch ++ tail sortedBranchesWithCost) (currentNode : exploredList)
  | otherwise = aStarSearch destination next heuristic cost (tail sortedBranchesWithCost) exploredList
        where sortedBranchesWithCost = sortBranchesWithCost branches heuristic cost
              currentBranch = head sortedBranchesWithCost
              currentNode = head currentBranch

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
minimax:: Game->Player->Int
minimax game player
  | terminal game = eval game
  | minPlayer player = minimum [minimax move (switch player) | move <- moves game player]
  | maxPlayer player = maximum [minimax move (switch player) | move <- moves game player] -- switch allows you to change between players



-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.


-- ALPHABETA RANGE IS (-2, +2)
-- Following the pseudo code on the assignment handout
alphabeta :: Game -> Player -> Int
alphabeta game player
  -- v <- maxValue(state, -inf, +inf)
  | maxPlayer player = maxValue game (-2) 2 -- -inf and +inf are set to -2 and +2 respectively
  | minPlayer player = minValue game (-2) 2

-- | Section 5.2 Wild Tic Tac Toe


-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
  | checkWin game 1 = 1
  | checkWin game 0 = 1
  | otherwise = 0


-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

-- CHANGES:
--    change eval to evalWild
--    change moves to movesWild
--    make terminal game in maxValueWild return (evalWild game * -1)
alphabetaWild :: Game -> Player -> Int
alphabetaWild game player
  | maxPlayer player = maxValueWild game (-2) 2
  | minPlayer player = minValueWild game (-2) 2




-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

-- optional
minimaxWild:: Game->Player->Int
minimaxWild game player =undefined





			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores


-- next

-- Outputs the possible locations after movement of the nodes
move :: Node -> Branch
move (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]


-- iterDeepSearch

-- Helper function to perform iterative deepening search according to the specified conditions
iterDeepSearch' :: Node -> (Branch -> [Branch]) -> [Branch] -> Int -> Maybe Branch
iterDeepSearch' destination next [] d = Nothing
iterDeepSearch' destination next branches d
 | elem destination badNodesList = Nothing
 | checkArrival destination currentNode = Just currentBranch
 | (notElem currentNode badNodesList) && (length currentBranch <= d) = iterDeepSearch' destination next (next currentBranch ++ tail branches) d
 | (notElem currentNode badNodesList) && (length currentBranch > d) && (d < maxDepth) = iterDeepSearch' destination next branches (d+1)
 | otherwise = iterDeepSearch' destination next branches d
    where currentBranch = head branches
          currentNode = head currentBranch


-- bestFirstSearch

-- compares the heuristc value of each branch (as tuples)
compareCost :: (Branch, Int) -> (Branch, Int) -> Ordering
compareCost (branch1,cost1) (branch2,cost2) = compare cost1 cost2

-- sorts the branches based on their heuristic values
sortBranches :: [Branch] -> (Node -> Int) -> [Branch]
sortBranches branches heuristic = [b | (b,c) <- sortBy compareCost (zip branches (map (heuristic . head) branches))]


-- aStarSearch

-- sorts the branches based on the added total of their heuristic value as well as their cost
sortBranchesWithCost :: [Branch] -> (Node -> Int) -> (Branch -> Int) -> [Branch]
sortBranchesWithCost branches heuristic cost = [b | (b,c) <- sortBy compareCost (zip branches (zipWith (+) (map (heuristic . head) branches) (map cost branches)))]


-- alphabeta
-- Functions are coded based on the pseudocode on the assignment handout (page 7 of A1.pdf)

maxValue :: Game -> Int -> Int -> Int
maxValue game alpha beta
  -- if ternminal-test(state) then return utility(state)
  | terminal game = eval game
  -- v <- -inf
  | otherwise = maxFor (moves game 1) alpha beta (-2)

maxFor :: [Game] -> Int -> Int -> Int -> Int
maxFor [] alpha beta v = v
maxFor (game:games) alpha beta v
  -- alpha <- max(alpha, v')
  | v' < beta = maxFor games (max alpha v') beta v'
  -- if v' >= beta then return v'
  | otherwise = v'
      -- v' <- max(v, minValue(result(s, a), alpha, beta))
      where v' = (max v (minValue game alpha beta))

minValue :: Game -> Int -> Int -> Int
minValue game alpha beta
  -- if terminal-test(state) then return utility(state)
  | terminal game = eval game
  -- v <- +inf
  | otherwise = minFor (moves game 0) alpha beta 2

minFor :: [Game] -> Int -> Int -> Int -> Int
minFor [] alpha beta v = v
minFor (game:games) alpha beta v
  -- beta <- min(beta, v')
  | v' > alpha = minFor games alpha (min beta v') v'
  -- if v' <= alpha then return v'
  | otherwise = v'
      -- v' = min(v, max-value(result(s, a), alpha, beta))
      where v' = (min v (maxValue game alpha beta))


-- alphabetaWild
-- Also following the pseudocode with minor changes indicated below

maxValueWild :: Game -> Int -> Int -> Int
maxValueWild game alpha beta
  | terminal game = (evalWild game) * (-1) -- make terminal game in maxValueWild return (evalWild game * -1)
  | otherwise = maxForWild (movesWild game 1) alpha beta (-2) -- change moves to movesWild

maxForWild :: [Game] -> Int -> Int -> Int -> Int
maxForWild [] alpha beta v = v
maxForWild (game:games) alpha beta v
  | v' < beta = maxForWild games (max alpha v') beta v'
  | otherwise = v'
      where v' = (max v (minValueWild game alpha beta))

minValueWild :: Game -> Int -> Int -> Int
minValueWild game alpha beta
  | terminal game = evalWild game -- change eval to evalWild
  | otherwise = minForWild (movesWild game 0) alpha beta 2 -- change moves to movesWild

minForWild :: [Game] -> Int -> Int -> Int -> Int
minForWild [] alpha beta v = v
minForWild (game:games) alpha beta v
  | v' > alpha = minForWild games alpha (min beta v') v'
  | otherwise = v'
      where v' = (min v (maxValueWild game alpha beta))
