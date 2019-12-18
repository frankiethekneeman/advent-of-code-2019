import System.Environment
import Data.Array
import Data.List
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
   readInput (ListIO i o)
     | otherwise = (head i, ListIO (tail i) o)
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

data Facing = Up | Down | Leftward | Rightward deriving (Eq, Show)

applyTurn :: Facing -> Char -> Facing
applyTurn Up 'L' = Leftward
applyTurn Up 'R' = Rightward
applyTurn Down 'L' = Rightward
applyTurn Down 'R' = Leftward
applyTurn Leftward 'L' = Down
applyTurn Leftward 'R' = Up
applyTurn Rightward 'L' = Up
applyTurn Rightward 'R' = Down

vec :: Facing -> (Int, Int)
vec Up = (0, -1)
vec Down = (0, 1)
vec Leftward = (-1, 0)
vec Rightward = (1, 0)

move :: Int -> Int -> Facing -> (Int, Int)
move x y f = let (dx, dy) = vec f in (x + dx, y + dy)

left :: Int -> Int -> Facing -> (Int, Int)
left x y f = move x y (applyTurn f 'L')

right :: Int -> Int -> Facing -> (Int, Int)
right x y f = move x y (applyTurn f 'R')

turnDir :: Int -> Int -> Facing -> ScaffoldMap -> Maybe Char
turnDir x y f m
  | uncurry isScaffold (left x y f) m = Just 'L'
  | uncurry isScaffold (right x y f) m = Just 'R'
  | otherwise = Nothing

follow :: Int -> Int -> Facing -> ScaffoldMap -> (Int, Int, Int)
follow x y f m
  | not $ isScaffold x' y' m = (x, y, 0)
  | otherwise = let (x'', y'', dist) = follow x' y' f m in (x'', y'', dist + 1)
  where (dx, dy) = vec f
        x' = x + dx
        y' = y + dy

calcPath :: Int -> Int -> Facing -> ScaffoldMap -> [(Char, Int)]
calcPath x y facing m = 
  case turnDir x y facing m of
    Nothing -> []
    Just turn -> 
      let facing' = applyTurn facing turn
          (x', y', dist) = follow x y facing' m
        in [(turn, dist)] ++ calcPath x' y' facing' m

findRobot :: ScaffoldMap -> (Int, Int, Facing)
findRobot m = 
  let p = [(x, y) | x <- indices (m ! 0), y <- indices m, elem (m ! y ! x) "><^v"]
      (x, y) = head p
      f = case m ! y ! x of
        '>' -> Rightward
        'v' -> Down
        '<' -> Leftward
        '^' -> Up
    in (x, y, f)

toIns :: (Char, Int) -> [Char]
toIns (c, i) = c:"," ++ (show i)

toRoutine :: [(Char,Int)] -> [Char]
toRoutine [x] = toIns x ++ "\n"
toRoutine (x:xs) = toIns x ++ "," ++ toRoutine xs

count :: Eq a => [a] -> [a] -> Int
count _ [] = 0
count term string
  | term `isPrefixOf` string = 1 + (count term $ drop l string)
  | otherwise = count term $ tail string
  where l = length term

mostValuableHead :: Eq a => [[a]] -> [a]
mostValuableHead pieces =
  let candidates = filter (not.null) $ inits $ head pieces
      scores = map (\c -> (length c) * ((sum $ map (count c) pieces) - 1)) candidates :: [Int]
    in fst $ head $ reverse $ sortOn snd $ zip candidates scores

splitList :: Eq a => [a] -> [a] -> ([a], [a])
splitList search target
  | not (search `isInfixOf` target) = (target, [])
  | otherwise =
    let tok = head search
        l = length search
        (begin, end) = span (/=tok) target
      in if take l end == search
        then (begin, drop l end)
        else let (part2, rest) = splitList search $ tail end
               in (begin ++ [tok] ++ part2, rest)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn token string
  | rest == [] && first == token = []
  | rest == [] = [first]
  | otherwise = first:(splitOn token rest)
  where (first, rest) = splitList token string

toMain :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)] -> [(Char,Int)] -> [Char]
toMain a b c [] = []
toMain a b c path
  | a `isPrefixOf` path = 'A':toMain a b c (drop (length a) path)
  | b `isPrefixOf` path = 'B':toMain a b c (drop (length b) path)
  | c `isPrefixOf` path = 'C':toMain a b c (drop (length c) path)

withCommasAndNewLine :: [Char] -> [Char]
withCommasAndNewLine [x] = x:"\n"
withCommasAndNewLine (x:xs) = x:"," ++ withCommasAndNewLine xs

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let program = parseProgram contents
  let result = o $ io $ executeProgram boot { memory = program }
  let scaffoldMap = toArrays result
  let (robotX, robotY, facing) = findRobot scaffoldMap
  let path = calcPath robotX robotY facing scaffoldMap
  let a = mostValuableHead [path]
  let butA = filter (not.null) $ splitOn a path
  let b = mostValuableHead butA
  let onlyCs = filter (not.null) $ concat $ map (splitOn b) butA
  let c = mostValuableHead onlyCs
  let main = withCommasAndNewLine $ toMain a b c path
  let inputStr = main ++ (concat $ map toRoutine [a, b, c]) ++ "n\n"
  let input = map fromEnum inputStr
  let wakeUpProgram = Map.insert 0 2 program
  let dust = o $ io $ executeProgram boot { memory = wakeUpProgram, io = ListIO input [] }
  print $ head dust
