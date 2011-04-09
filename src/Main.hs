{--------------------------------------------
 Module Main
 main module for the bf interpreter
 Andrei de A. Formiga, 2004-05-31
 --------------------------------------------}
 
module Main where

import BFInterp
import System

-- main tests
main = do 
        args <- getArgs
        if not (null args) then
           do startState <- newState
              commands <- readFile (head args)
              eval commands startState
              putStr "\n"
         else
           do prog <- getProgName
              putStrLn ("Usage: " ++ prog ++ " <program file>")
