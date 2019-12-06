import System.Environment
import Data.Array

within :: (Ix i) => i -> Array i e -> Bool
within idx arr = 
  let (mn, mx) = bounds arr
    in mn <= idx && idx <= mx

type Memory = Array Int Int

data Computer = Computer {
  memory :: Array Int Int
  , insPointer :: Int
  , finished :: Bool
  , input :: [Int]
  , output :: [Int]
} deriving (Show)

write :: Int -> Int -> Computer -> Computer
write address value computer
  | address `within` mem = computer { memory = mem // [(address, value)] }
  where mem = memory computer

rd :: Computer -> (Int, Computer)
rd computer = 
  let value:remaining = input computer
    in (value, computer { input = remaining })

prnt :: Int -> Computer -> Computer
prnt value computer = computer { output = (value:(output computer)) }

advance :: Int -> Computer -> Computer
advance instructions computer = 
  let oldPointer = insPointer computer
    in computer { insPointer = oldPointer + instructions }

getInstruction :: Computer -> Int
getInstruction (Computer memory insPointer _ _ _) = memory ! insPointer

getParams :: Int -> Computer -> [Int]
getParams count (Computer memory insPointer _ _ _) = [ memory ! (insPointer + i) | i <- [1..count]]

data ParamMode = Position | Immediate deriving (Enum, Show, Eq)

data Parameter = Parameter {
  mode :: ParamMode
  , param :: Int
} deriving (Show)

getValue :: Parameter -> Computer -> Int
getValue (Parameter Immediate val) _ = val
getValue (Parameter Position address) computer = (memory computer) ! address

type OpFunc = [Int] -> Computer -> Computer

mathOp :: (Int -> Int -> Int) -> [Int] -> Computer -> Computer
mathOp f [lhs, rhs, address] computer = write address (lhs `f` rhs) computer

add :: OpFunc
add args computer = mathOp (+) args computer

mult :: OpFunc
mult args computer = mathOp (*) args computer

inputOp :: OpFunc
inputOp [address] computer = 
    let (value, postRead) = rd computer
      in write address value postRead

outputOp :: OpFunc
outputOp [value] computer = prnt value computer

finish :: OpFunc
finish [] computer = computer { finished = True }

data Operation = Operation {
  modalParams
  , numParams :: Int
  , f :: OpFunc
}

writeMode :: ParamMode -> ParamMode
writeMode Position = Immediate
getModes :: Int -> Operation -> [ParamMode]
getModes instruction (Operation modal total _) = 
    let bits = reverse (show (instruction `quot` 100)) ++ repeat '0'
        ints = [ read [i] :: Int | i <- bits ]
        asEnum = [ toEnum i :: ParamMode | i <- ints ]
        actual = (take modal asEnum) ++ (map writeMode (drop modal asEnum))
      in take total actual

getOp :: Int -> Operation
getOp  1 = Operation 2 3 add
getOp  2 = Operation 2 3 mult
getOp  3 = Operation 0 1 inputOp
getOp  4 = Operation 1 1 outputOp
getOp 99 = Operation 0 0 finish
getOp code
  | code > 99 = getOp (code `mod` 100)

parseProgram :: String -> Memory
parseProgram str = 
  let spaceSeparated = [if c == ',' then ' ' else c | c <- str]
      instructions = [read i :: Int | i <- words spaceSeparated]
      memNeeded = length instructions
      lastAddress = memNeeded - 1
    in array (0, lastAddress) (take memNeeded (zip [0..] instructions))

executeInstruction :: Computer -> Computer
executeInstruction computer = 
  let instruction = getInstruction computer
      op = getOp instruction
      paramCt = numParams op
      paramValues = getParams paramCt computer
      paramModes = getModes instruction op
      parameters = map (((flip getValue) computer).(uncurry Parameter)) (zip paramModes paramValues)
      afterOp = (f op) parameters computer
    in if finished afterOp then afterOp else advance (paramCt + 1) afterOp

executeProgram :: Computer -> Computer
executeProgram comp
  | fin = comp
  | otherwise = executeProgram (executeInstruction comp)
  where fin = finished comp

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let initialState = Computer (parseProgram contents) 0 False [1] []
  let finalState = executeProgram initialState
  let failedTests = filter (/=0) (tail (output finalState))
  if length failedTests > 0 then print (failedTests) else print (head (output finalState))
