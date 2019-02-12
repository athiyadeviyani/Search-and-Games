module Main where

import PlayGame (playGameMain)
import RobotPath (robotPathMain)

main :: IO ()
main = do
  putStrLn "Which functions would you like to check?"
  putStrLn "1. Search functions"
  putStrLn "2. Game functions"
  putStrLn "3. (Neither, I want to quit this program)"
  getInt >>= (\i -> case i of
                      1 -> robotPathMain
                      2 -> playGameMain
                      3 -> putStrLn "Ok, goodbye."
                      (_) -> do putStrLn "That wasn't an option ... please try again."
                                main)
      where getInt = getLine >>= (\str -> ((readIO str)::IO Int))
       

