import System.Environment
import qualified Data.Map.Strict as Map
import Data.List
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
   readInput (ListIO (i:is) o) = (i, ListIO is o)
   writeOutput v (ListIO i o) = ListIO i (v:o)

type Probe = Int -> Int -> Int

probe :: Memory -> Int -> Int -> Int
probe memory x y = (head.o.io.executeProgram) boot {memory = memory, io = ListIO [x,y] []}

findStartingPoint :: Probe -> Int -> Int
findStartingPoint p x = fst.head $ dropWhile ((==0).snd) $ zip [0..] $ map (p x) [0..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l):(chunksOf n (drop n l))

santaSize :: Int
santaSize = 100

santaFits :: Probe -> Int -> Int -> Bool
santaFits p x y = (p x y == 1) && (p (x - (santaSize - 1)) (y + (santaSize - 1)) == 1)

canMove :: Probe -> (Int, Int) -> (Int, Int) -> Bool
canMove p (x, y) (dx, dy) = santaFits p (x + dx) (y + dy)

binSearch :: (Int -> Int) -> Int -> Int -> Int
binSearch test min max
  | min + 1 == max = min
  | otherwise = let val = (min + max) `div` 2
                  in if test val == 1
                    then binSearch test val max
                    else binSearch test min val

walkDown :: Probe -> (Int, Int) -> (Int, Int)
walkDown p (x, y) = (x, binSearch (p x) y (2 * y))

walkRight :: Probe -> (Int, Int) -> (Int, Int)
walkRight p (x, y) = (binSearch (`p` y) x (2 * x), y)

findFit :: Probe -> (Int, Int) -> (Int, Int)
findFit p (x, y)
  | santaFits p x y = (x, y)
  | otherwise = findFit p $ walkRight p $ walkDown p (x, y)


maxDelta :: Int
maxDelta = santaSize

deltas :: [(Int, Int)]
deltas = reverse $ sortOn (\(dx, dy) -> dx + dy) [(-dx, -dy) | dx <- [0..maxDelta], dy <- [0..maxDelta], dx + dy /= 0]

perfectFit :: Probe -> (Int, Int) -> (Int, Int)
perfectFit pr (x,y) =
  let attempts = map fst $ dropWhile (not.snd) $ zip deltas $ map (canMove pr (x, y)) deltas
    in case attempts of
      [] -> (x, y)
      (dx, dy):_ -> perfectFit pr (x + dx, y + dy)

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let program = parseProgram contents
  let probeInst = probe program
  let effects  = [probeInst x y  | y <- [0..49], x <- [0..49]]
  let startX = santaSize
  let start = (startX, findStartingPoint probeInst startX)
  let (urx, ury) = perfectFit probeInst $ findFit probeInst start
  let ulx = urx - (santaSize - 1)
  let uly = ury
  print $ (ulx * 10000) + uly
