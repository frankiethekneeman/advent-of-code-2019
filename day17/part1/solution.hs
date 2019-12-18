import System.Environment
import Data.Array
import qualified Data.Map.Strict as Map
import Debug.Trace

class VirtualIO a where
  readInput :: a -> (Int, a)
  readInput _ = error "No input function supplied"
  writeOutput :: Int -> a -> a
  writeOutput _ _ = error "No output function supplied"

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

type Row = Array Int Char
type ScaffoldMap = Array Int Row

toArrays :: [Int] -> ScaffoldMap
toArrays output =
  let asChars = map (toEnum) $ reverse output
      broken = lines asChars
      l = length $ head broken
      sameSize = takeWhile ((==l).length) broken
      maxX = l - 1
      maxY = (length $ sameSize) - 1
    in listArray (0,maxY) $ map (listArray (0,maxX)) sameSize

isScaffold :: Int -> Int -> ScaffoldMap -> Bool
isScaffold x y m
  -- | trace (show (x,y, bounds m, bounds (m!0))) False = undefined
  | x < (fst $ bounds $ m ! 0) = False
  | x > (snd $ bounds $ m ! 0) = False
  | y < (fst $ bounds m) = False
  | y > (snd $ bounds m) = False
  | otherwise = (m ! y) ! x == '#'

isIntersection :: Int -> Int -> ScaffoldMap -> Bool
isIntersection x y m = 
  let up = isScaffold x (y-1) m
      down = isScaffold x (y+1) m
      left = isScaffold (x-1) y m
      right = isScaffold (x+1) y m
    in up && down && left && right && isScaffold x y m
     

findIntersections :: ScaffoldMap -> [(Int, Int)]
findIntersections m = 
  [(x, y) | x <- indices (m ! 0), y <- indices m, isIntersection x y m]


main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let program = parseProgram contents
  let result = o $ io $ executeProgram boot { memory = program }
  let scaffoldMap = toArrays result
  let intersections = findIntersections scaffoldMap
  let alignments = map (uncurry (*)) intersections
  print $ sum alignments
