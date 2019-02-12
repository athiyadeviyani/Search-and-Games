module RobotPath (robotPathMain) where

import Data.Array
import Data.Maybe (fromJust,isNothing)
import System.IO
import Control.Exception
import Inf2d1

newtype Path = Path ()

instance Show Path where
    show pt = "x"

depthLimit = 40
gridLength::Int
gridLength = 6
gridWidth :: Int
gridWidth = 6

initRobotGrid :: Array (Int,Int) (Maybe Path)
initRobotGrid = array ((1,1),(gridLength,gridWidth)) [((i,j), Nothing) | i<-[1..gridLength], j<-[1..gridWidth]]

showRP :: Array (Int,Int) (Maybe Path) -> String
showRP grid = showGrid gridLength gridWidth grid

robotPathMain = do
  start <- getGridPt " a start "
  end <- getGridPt " an end "
  method <- getMethod
  findPath start end method

findPath :: (Int,Int) -> (Int,Int) -> (Int,Int) -> IO ()
findPath start end method = putStrLn $ showRP $ makePath initRobotGrid path
    where search = case method of
                   (1,_) -> breadthFirstSearch end next [[start]] [] 
                   (2,_) -> depthFirstSearch end next [[start]] []
                   (3,_) -> depthLimitedSearch end next [[start]] depthLimit
                   (4,_) -> iterDeepSearch  end next start 1
                   (5,_) -> bestFirstSearch end next (manhattan end) [[start]] []
                   (6,_) -> aStarSearch end next (manhattan end) cost [[start]] []
          path = case search of
                   Just x -> x
                   Nothing -> error "Your search function couldn't find a path."

makePath :: Array (Int,Int) (Maybe Path) -> Branch -> Array (Int,Int) (Maybe Path)
makePath arr path = arr//(map (\x -> (x,Just $ Path ())) path)

getMethod :: IO (Int,Int)
getMethod = do
  putStrLn "Which search method would you like to use?"
  putStrLn "1. Breadth First Search"
  putStrLn "2. Depth First Search"
  putStrLn ("3. Depth Limited Search (Test at a depth limit of " ++ show(depthLimit) ++ ")")
  putStrLn "4. Iterative Deepening Search"
  putStrLn "5. Best First Search"
  putStrLn "6. A* Search"
  num <- getNum
  case num of
    1 -> return (1,0)
    2 -> return (2,0)
    3 -> return (3,0)
    4 -> return (4,0)
    5 -> return (5,0)
    6 -> return (6,0)
    (_) -> do putStrLn "Please enter a number in the range [1-6]"
              getMethod

getNum :: IO Int
getNum = do
  str <- getLine
  num <- catch((readIO str)::IO Int)
          (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException) ++ ".\nPlease enter a valid Int value:")
                    getNum)
  if not (elem num [1..6])
     then do putStrLn "Please enter a number in the range [1-6]"
             getNum
     else return num

getHeur :: IO Int
getHeur = do
  putStrLn "Which heuristic do you want to use?"
  putStrLn "1. Manhattan distance"
  putStrLn "2. Simplified Model Heuristic"
  str <- getLine
  num <- catch((readIO str)::IO Int)
         (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException) ++ ".\nPlease enter a valid Int value:")
                   getHeur)
  if not (elem num [1..2])
     then do putStrLn "Please enter a number in the range [1-2]"
             getHeur
     else return num

getGridPt :: String -> IO (Int,Int)
getGridPt name = do
  putStrLn $ "Please enter" ++ name ++ "grid point (row, column):"
  str <- getLine
  pt <- catch((readIO str)::IO (Int,Int))
        (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException))
                  getGridPt name)
  if not (elem pt [(i,j) | i <- [1..gridLength], j <- [1..gridWidth]])
   then do putStrLn ("Point needs to be between (1,1) and (" ++ show(gridLength) ++ "," ++ show(gridWidth) ++ ")")
           getGridPt name
   else return pt


showGrid :: Show a => Int -> Int -> Array (Int,Int) (Maybe a) -> String
showGrid r c board = foldl1 g $ map (showRow.row) [1..r]
    where row n = filter ((==n).(fst.fst)) $ assocs board
          showRow rw = " " ++ (foldl1 f $ map (showSquare.snd) rw) ++ "\n"
          f x y = x ++ " | " ++ y
          g x y = x ++ concat (replicate c " -  ") ++ "\n" ++ y
          showSquare x | isNothing x = " "
                       | otherwise = show $ fromJust x
