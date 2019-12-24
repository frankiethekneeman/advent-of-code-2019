import System.Environment
import Data.Array
import Data.List
import qualified Data.Array.IArray as IA
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

data ListIO = ListIO {
    i :: [Int]
    , o :: [Int]
} deriving (Show)

instance VirtualIO ListIO where
   readInput (ListIO i o) = (head i, ListIO (tail i) o)
   writeOutput v (ListIO i o) = ListIO i (v:o)

data Packet = Packet {
   destination :: Int
   , x :: Int
   , y :: Int
}

data NicIO = NicIO {
    input :: [Int]
    , output :: [Packet]
    , dst :: Maybe Int
    , xCoord :: Maybe Int
}

instance VirtualIO NicIO where
  readInput io@NicIO{input = []} = (-1, io)
  readInput io@NicIO{input = i} = (head i,  io{input = (tail i)})
  writeOutput d io@NicIO{dst = Nothing} = io{dst = Just d}
  writeOutput x io@NicIO{xCoord = Nothing} = io{xCoord = Just x}
  writeOutput y (NicIO i o (Just d) (Just x))= NicIO i (o ++ [Packet d x y]) Nothing Nothing

newNic :: Int -> NicIO
newNic address = NicIO [address] [] Nothing Nothing

type Network = Array Int (Computer NicIO)

initNetwork :: Int -> Memory -> Network
initNetwork n program = array (0, n-1) $ take n addressed
  where nics = map newNic [0..]
        vms = map (\nic -> boot {io = nic, memory = program}) nics
        addressed = zip [0..] vms

clearOutput :: Computer NicIO -> Computer NicIO
clearOutput c@Computer{io=io@NicIO{output=_}} = c{io = io{output=[]}}

step :: Network -> ([Packet], Network)
step network = (packets, network')
  where afterIns = IA.amap executeInstruction network
        packets = concat.elems $ IA.amap (output.io) afterIns
        network' = IA.amap clearOutput afterIns

deliver :: Network -> Packet -> Network
deliver network (Packet d x y) = network'
   where target = network ! d
         nic@NicIO{input = i} = io target
         network' = network // [(d, target{io = nic{input = i ++ [x, y]}})]

getAnswer :: Network -> Int
getAnswer network = case undeliverablePackets of
    [(Packet 255 _ y)] -> y
    [] -> getAnswer network'
  where (packets, awaitingDelivery) = step network
        (minAddress, maxAddress) = bounds network
        (deliverablePackets, undeliverablePackets) = partition (\(Packet d _ _) -> d <= maxAddress && d >= minAddress) packets
        network' = foldl deliver awaitingDelivery deliverablePackets

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let program = parseProgram contents
  let network = initNetwork 50 program
  print $ getAnswer network
