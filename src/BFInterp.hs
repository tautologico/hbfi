{--------------------------------------------
  Module BFInterp
  A basic bf interpreter 
  Andrei de A. Formiga, 2004-05-31
 --------------------------------------------}

module BFInterp (State(), newState, eval) where

import Data.Array.IO
import Char

type MemoryPointer = Int
type Memory = IOUArray MemoryPointer Int
type Instructions = String
type Stack = [Instructions]
type Processor = State -> IO State

data State = State MemoryPointer Memory Instructions Stack
data CommandProcessor = Proc Char Processor

-- size of the cell array
arraysize :: Int
arraysize = 30000


-- list of command processors
processors = [ 	(Proc '>' incrementPtr), 
				(Proc '<' decrementPtr),
				(Proc '+' incrementCell),
				(Proc '-' decrementCell),
				(Proc '.' outputCellValue),
				(Proc ',' inputCellValue),
				(Proc '[' openingBracket),
				(Proc ']' closingBracket) ]

createMemory :: IO Memory
createMemory = newArray (0, arraysize) 0

-- create a new intepreter state
newState :: IO State
newState = do mem <- createMemory
              return (State 0 mem "" [])

-- set the value of a memory cell
setMemCell :: Memory -> Int -> Int -> IO ()
setMemCell mem ind val = writeArray mem ind val

-- get the value of a memory cell
getMemCell :: Memory -> Int -> IO Int
getMemCell mem ind = readArray mem ind

--- jump all instructions up to a matching closing bracket
jumpInstructions :: String -> String
jumpInstructions str = jumpInstAux str 0
	where jumpInstAux [] _ = []				-- TODO [1] change to error ?
	      jumpInstAux (']' : rest) 0 = rest
	      jumpInstAux (']' : rest) n = jumpInstAux rest (n - 1)
	      jumpInstAux ('[' : rest) n = jumpInstAux rest (n + 1)
	      jumpInstAux (ch : rest) n = jumpInstAux rest n


{-------------
   command processors
         --------------}

-- increment the value of the cell currently at pointer
incrementCell :: Processor
incrementCell st@(State ptr mem inst stk) = do val <- getMemCell mem ptr
                                               let newval = val + 1
                                               setMemCell mem ptr newval
                                               return st

-- decrement the value of the cell currently at pointer
decrementCell :: Processor
decrementCell st@(State ptr mem inst stk) = do val <- getMemCell mem ptr
                                               let newval = val - 1
                                               setMemCell mem ptr newval
                                               return st
	
-- TODO [1] add error conditions
incrementPtr :: Processor
incrementPtr (State ptr mem inst stk) = return (State (ptr + 1) mem inst stk)

-- TODO [1] add error conditions (ptr = 0)
decrementPtr :: Processor
decrementPtr (State ptr mem inst stk) = return (State (ptr - 1) mem inst stk)

-- get the value for the current cell from input
inputCellValue :: Processor
inputCellValue st@(State ptr mem inst stk) = 
                       do valCh <- getChar
                          let val = fromEnum valCh
                          setMemCell mem ptr val
                          return st
                          
-- output the value of the current cell
outputCellValue :: Processor
outputCellValue st@(State ptr mem inst stk) = 
						do val <- getMemCell mem ptr
						   putChar (toEnum val)
						   return st

-- process an opening bracket
openingBracket :: Processor
openingBracket (State ptr mem inst stk) = 
                do val <- getMemCell mem ptr
                   let jmp = val == 0
                   if jmp then
                       let newinst = jumpInstructions inst in
                       return (State ptr mem newinst stk)
                    else
                       return (State ptr mem inst (inst : stk))

-- process a closing bracket
closingBracket :: Processor
closingBracket (State ptr mem inst []) = error "unmatched ]"
closingBracket (State ptr mem inst stk@(loc : rest)) =
                do val <- getMemCell mem ptr
                   let jmp = val /= 0
                   if jmp then
                      return (State ptr mem loc stk)
                    else
                      return (State ptr mem inst rest)

-- identity processor
idProcessor :: Processor
idProcessor st = return st

-- find a suitable command processor based on the command char
findProcessor :: [CommandProcessor] -> Char -> Processor
findProcessor [] ch = idProcessor
findProcessor ((Proc cmdch proc) : rest) ch
             | cmdch == ch  = proc
             | otherwise    = findProcessor rest ch


-- process the given command
processCommand :: Char -> State -> IO State
processCommand ch state = let proc = findProcessor processors ch in
						  	proc state


{----------
   the evaluator 
       -----------}
       
-- the main eval function
eval                        :: Instructions -> State -> IO State
eval cmds (State ptr mem _ _) = evalAux (State ptr mem cmds [])
      
-- auxiliar eval                       
evalAux						  :: State -> IO State
evalAux st@(State ptr mem [] stk) = 
                       do return st
evalAux st@(State ptr mem (cmd : rest) stk) =
                       let stAdv = (State ptr mem rest stk) in
                       do newst <- processCommand cmd stAdv
                          evalAux newst

