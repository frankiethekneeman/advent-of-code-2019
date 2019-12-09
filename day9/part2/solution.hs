import System.Environment
import qualified Data.Map.Strict as Map
import Data.List

type Memory = Map.Map Int Int
data State = Running | Halted | Paused deriving (Enum, Show, Eq)

data Computer = Computer {
  memory :: Memory
  , insPointer :: Int
  , state :: State
  , input :: [Int]
  , output :: [Int]
  , relativeBase :: Int
} deriving (Show)

pauseOnOutput :: Bool
pauseOnOutput = False

boot :: Computer
boot = Computer Map.empty 0 Running [] [] 0

shouldBreak :: Computer -> Bool
shouldBreak (Computer _ _ Running _ _ _) = False
shouldBreak x = True

readMem :: Int -> Memory -> Int
readMem = Map.findWithDefault 0

write :: Int -> Int -> Computer -> Computer
write address value computer
  | address >= 0 = computer { memory = Map.insert address value mem }
  where mem = memory computer

rd :: Computer -> (Int, Computer)
rd computer = 
  let value:remaining = input computer
    in (value, computer { input = remaining })

prnt :: Int -> Computer -> Computer
prnt value computer = computer { 
    output = (value:(output computer)) 
    , state = if pauseOnOutput then Paused else state computer}

advance :: Int -> Computer -> Computer
advance instructions computer = 
  let oldPointer = insPointer computer
    in computer { insPointer = oldPointer + instructions }

getInstruction :: Computer -> Int
getInstruction (Computer memory insPointer _ _ _ _) = readMem insPointer memory

getParams :: Int -> Computer -> [Int]
getParams count (Computer memory insPointer _ _ _ _) = [ readMem (insPointer + i) memory | i <- [1..count]]

data ParamMode = Position | Immediate | Relative | RelWrite deriving (Enum, Show, Eq)

data Parameter = Parameter {
  mode :: ParamMode
  , param :: Int
} deriving (Show)

getValue :: Parameter -> Computer -> Int
getValue (Parameter Immediate val) _ = val
getValue (Parameter Position address) computer = readMem address (memory computer)
getValue (Parameter Relative offset) (Computer mem _ _ _ _ base) = readMem (base + offset) mem
getValue (Parameter RelWrite offset) (Computer mem _ _ _ _ base) = base + offset

type OpFunc = [Int] -> Computer -> Computer

mathOp :: (Int -> Int -> Int) -> [Int] -> Computer -> Computer
mathOp f [lhs, rhs, address] = write address (lhs `f` rhs)

add :: OpFunc
add = mathOp (+)

mult :: OpFunc
mult = mathOp (*)

inputOp :: OpFunc
inputOp [address] computer = 
    let (value, postRead) = rd computer
      in write address value postRead

outputOp :: OpFunc
outputOp [value] = prnt value

halt :: OpFunc
halt [] computer = computer { state = Halted }

cndJmp :: Bool -> [Int] -> Computer -> Computer
cndJmp goal [arg, target] computer
  | (0 /= arg) == goal = computer { insPointer = target - 3 }
  | otherwise = computer

jmpT :: OpFunc
jmpT = cndJmp True

jmpF :: OpFunc
jmpF = cndJmp False

cmpOp :: (Int -> Int -> Bool) -> [Int] -> Computer -> Computer
cmpOp cmp = mathOp (curry (fromEnum.(uncurry cmp))) 

lt :: OpFunc
lt = cmpOp (<)

eq :: OpFunc
eq = cmpOp (==)

adjBase :: OpFunc
adjBase [offset] computer = computer { relativeBase = offset + (relativeBase computer) }

data Operation = Operation {
  modalParams
  , numParams :: Int
  , f :: OpFunc
}

writeMode :: ParamMode -> ParamMode
writeMode Position = Immediate
writeMode Relative = RelWrite

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
getOp  5 = Operation 2 2 jmpT
getOp  6 = Operation 2 2 jmpF
getOp  7 = Operation 2 3 lt
getOp  8 = Operation 2 3 eq
getOp  9 = Operation 1 1 adjBase
getOp 99 = Operation 0 0 halt
getOp code
  | code > 99 = getOp (code `mod` 100)

parseProgram :: String -> Memory
parseProgram str = 
  let spaceSeparated = [if c == ',' then ' ' else c | c <- str]
      instructions = [read i :: Int | i <- words spaceSeparated]
    in Map.fromDistinctAscList $ zip [0..] instructions

executeInstruction :: Computer -> Computer
executeInstruction computer = 
  let instruction = getInstruction computer
      op = getOp instruction
      paramCt = numParams op
      paramValues = getParams paramCt computer
      paramModes = getModes instruction op
      parameters = map (((flip getValue) computer).(uncurry Parameter)) (zip paramModes paramValues)
      afterOp = (f op) parameters computer
    in advance (paramCt + 1) afterOp

executeProgram :: Computer -> Computer
executeProgram comp
  | fin = comp
  | otherwise = executeProgram (executeInstruction comp)
  where fin = shouldBreak comp

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let program = parseProgram contents
  let result = output $ executeProgram boot { memory = program, input = [2] }
  if length result /= 1
    then print ("ERROR! " ++ (show result))
    else print $ head result
