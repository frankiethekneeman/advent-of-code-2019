import System.Environment
import qualified Data.Map.Strict as Map
import Data.List

class VirtualIO a where
  readInput :: a -> (Int, a)
  writeOutput :: Int -> a -> a

type Memory = Map.Map Int Int
data State = Running | Halted | Paused deriving (Enum, Show, Eq)

data Computer a = Computer {
  memory :: Memory
  , insPointer :: Int
  , state :: State
  , relativeBase :: Int
  , io :: a
} deriving (Show)

pauseOnOutput :: Bool
pauseOnOutput = False

boot :: Computer ListIO
boot = Computer Map.empty 0 Running 0 (ListIO [] [])

shouldBreak :: Computer a -> Bool
shouldBreak (Computer _ _ Running _ _) = False
shouldBreak x = True

readMem :: Int -> Memory -> Int
readMem = Map.findWithDefault 0

write :: Int -> Int -> Computer a -> Computer a
write address value computer
  | address >= 0 = computer { memory = Map.insert address value mem }
  where mem = memory computer

rd :: VirtualIO a => Computer a -> (Int, Computer a)
rd computer = 
  let (value, remaining) = readInput $ io computer
    in (value, computer { io = remaining })

prnt :: VirtualIO a => Int -> Computer a -> Computer a
prnt value computer = computer { 
    io = writeOutput value $ io computer
    , state = if pauseOnOutput then Paused else state computer
  }

advance :: Int -> Computer a -> Computer a
advance instructions computer = 
  let oldPointer = insPointer computer
    in computer { insPointer = oldPointer + instructions }

getInstruction :: Computer a -> Int
getInstruction (Computer memory insPointer _ _ _) = readMem insPointer memory

getParams :: Int -> Computer a -> [Int]
getParams count (Computer memory insPointer _ _ _) = [ readMem (insPointer + i) memory | i <- [1..count]]

data ParamMode = Position | Immediate | Relative | RelWrite deriving (Enum, Show, Eq)

data Parameter = Parameter {
  mode :: ParamMode
  , param :: Int
} deriving (Show)

getValue :: Parameter -> Computer a -> Int
getValue (Parameter Immediate val) _ = val
getValue (Parameter Position address) computer = readMem address (memory computer)
getValue (Parameter Relative offset) (Computer mem _ _ base _) = readMem (base + offset) mem
getValue (Parameter RelWrite offset) (Computer mem _ _ base _) = base + offset

type OpFunc a = [Int] -> Computer a -> Computer a

mathOp :: (Int -> Int -> Int) -> [Int] -> Computer a -> Computer a
mathOp f [lhs, rhs, address] = write address (lhs `f` rhs)

add :: OpFunc a
add = mathOp (+)

mult :: OpFunc a
mult = mathOp (*)

inputOp :: VirtualIO a => OpFunc a
inputOp [address] computer = 
    let (value, postRead) = rd computer
      in write address value postRead

outputOp :: VirtualIO a => OpFunc a
outputOp [value] = prnt value

halt :: OpFunc a
halt [] computer = computer { state = Halted }

cndJmp :: Bool -> [Int] -> Computer a -> Computer a
cndJmp goal [arg, target] computer
  | (0 /= arg) == goal = computer { insPointer = target - 3 }
  | otherwise = computer

jmpT :: OpFunc a
jmpT = cndJmp True

jmpF :: OpFunc a
jmpF = cndJmp False

cmpOp :: (Int -> Int -> Bool) -> [Int] -> Computer a -> Computer a
cmpOp cmp = mathOp (curry (fromEnum.(uncurry cmp))) 

lt :: OpFunc a
lt = cmpOp (<)

eq :: OpFunc a
eq = cmpOp (==)

adjBase :: OpFunc a
adjBase [offset] computer = computer { relativeBase = offset + (relativeBase computer) }

data Operation a = Operation {
  modalParams
  , numParams :: Int
  , f :: OpFunc a
}

writeMode :: ParamMode -> ParamMode
writeMode Position = Immediate
writeMode Relative = RelWrite

getModes :: Int -> Operation a -> [ParamMode]
getModes instruction (Operation modal total _) = 
    let bits = reverse (show (instruction `quot` 100)) ++ repeat '0'
        ints = [ read [i] :: Int | i <- bits ]
        asEnum = [ toEnum i :: ParamMode | i <- ints ]
        actual = (take modal asEnum) ++ (map writeMode (drop modal asEnum))
      in take total actual

getOp :: VirtualIO a => Int -> Operation a
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

executeInstruction :: VirtualIO a => Computer a -> Computer a
executeInstruction computer = 
  let instruction = getInstruction computer
      op = getOp instruction
      paramCt = numParams op
      paramValues = getParams paramCt computer
      paramModes = getModes instruction op
      parameters = map (((flip getValue) computer).(uncurry Parameter)) (zip paramModes paramValues)
      afterOp = (f op) parameters computer
    in advance (paramCt + 1) afterOp

executeProgram :: VirtualIO a => Computer a -> Computer a
executeProgram comp
  | fin = comp
  | otherwise = executeProgram (executeInstruction comp)
  where fin = shouldBreak comp

data ListIO = ListIO {
    i :: [Int]
    , o :: [Int]
} deriving (Show)

instance VirtualIO ListIO where
   readInput (ListIO i o) = (head i, ListIO (tail i) o)
   writeOutput v (ListIO i o) = ListIO i (v:o)

data RobotMode = Paint | Turn deriving (Eq, Show)
data HullRobotIO = HullRobotIO {
   rMode :: RobotMode
   , position :: (Int, Int)
   , facing :: (Int, Int)
   , tiles :: Map.Map (Int, Int) Int
}
hullRobotIO :: HullRobotIO
hullRobotIO = HullRobotIO Paint (0,0) (0,1) Map.empty

move :: HullRobotIO -> HullRobotIO
move h =
  let (x, y) = position h
      (dx, dy) = facing h
    in h { position = (x + dx, y + dy) }

turn :: Int -> HullRobotIO -> HullRobotIO
turn 1 h = let (dx, dy) = facing h in h { facing = (dy, -dx) }
turn 0 h = let (dx, dy) = facing h in h { facing = (-dy, dx) }

paint :: Int -> HullRobotIO -> HullRobotIO
paint color h = 
  let t = tiles h
      p = position h
    in h { tiles = Map.insert p color t }


currentColor :: HullRobotIO -> Int
currentColor HullRobotIO { tiles=t, position=p } = Map.findWithDefault 0 p t

instance VirtualIO HullRobotIO where
  readInput h = (currentColor h, h)
  writeOutput value h = case rMode h of
    Paint -> (paint value h) { rMode = Turn }
    Turn -> (move (turn value h)) { rMode = Paint }

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let program = parseProgram contents
  let result = tiles $ io $ executeProgram boot { memory = program, io = hullRobotIO }
  print $ Map.size result
