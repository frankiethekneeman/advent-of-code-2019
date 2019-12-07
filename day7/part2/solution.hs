import System.Environment
import Data.Array
import Data.List

within :: (Ix i) => i -> Array i e -> Bool
within idx arr = 
  let (mn, mx) = bounds arr
    in mn <= idx && idx <= mx

type Memory = Array Int Int
data State = Running | Halted | Paused deriving (Enum, Show, Eq)

data Computer = Computer {
  memory :: Array Int Int
  , insPointer :: Int
  , state :: State
  , input :: [Int]
  , output :: [Int]
} deriving (Show)

shouldBreak :: Computer -> Bool
shouldBreak (Computer _ _ Running _ _) = False
shouldBreak x = True

write :: Int -> Int -> Computer -> Computer
write address value computer
  | address `within` mem = computer { memory = mem // [(address, value)] }
  where mem = memory computer

rd :: Computer -> (Int, Computer)
rd computer = 
  let value:remaining = input computer
    in (value, computer { input = remaining})

prnt :: Int -> Computer -> Computer
prnt value computer = computer { output = (value:(output computer)), state = Paused }

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

halt :: OpFunc
halt [] computer = computer { state = Halted }

cndJmp :: Bool -> [Int] -> Computer -> Computer
cndJmp goal [arg, target] computer
  | (0 /= arg) == goal = computer { insPointer = target - 3 }
  | otherwise = computer

jmpT :: OpFunc
jmpT args computer = cndJmp True args computer

jmpF :: OpFunc
jmpF args computer = cndJmp False args computer

cmpOp :: (Int -> Int -> Bool) -> [Int] -> Computer -> Computer
cmpOp cmp args computer = mathOp (curry (fromEnum.(uncurry cmp))) args computer

lt :: OpFunc
lt args computer = cmpOp (<) args computer

eq :: OpFunc
eq args computer = cmpOp (==) args computer

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
getOp  5 = Operation 2 2 jmpT
getOp  6 = Operation 2 2 jmpF
getOp  7 = Operation 2 3 lt
getOp  8 = Operation 2 3 eq
getOp 99 = Operation 0 0 halt
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
    in advance (paramCt + 1) afterOp

executeProgram :: Computer -> Computer
executeProgram comp
  | fin = comp
  | otherwise = executeProgram (executeInstruction comp)
  where fin = shouldBreak comp

amplify :: Computer -> Computer -> Computer
amplify prev next =
  let signal = head $ output prev
      prevIn = input next
    in executeProgram $ next { input = prevIn ++ [signal], state = Running }

ampLoop :: [Computer] -> Int -> Int
ampLoop amplifiers signal = 
  let base =  Computer (array (0,0) [(0, 99)]) 0 Halted [] [signal]
      [a, b, c, d, e] = tail $ scanl amplify base amplifiers
      loopState = state e
      newSignal = head.output $ e
    in if loopState == Halted
      then newSignal
      else ampLoop [a, b, c, d, e] newSignal

initialize :: Int -> Memory -> Computer
initialize phase program = 
  Computer program 0 Running [phase] [] 

checkAmplifiers :: [Int] -> Memory -> Int
checkAmplifiers phases program =
  let amplifiers = map ((flip initialize) program) phases
    in ampLoop amplifiers 0

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let program = parseProgram contents
  print $ maximum $ map ((flip checkAmplifiers) program) $ permutations [5..9]
