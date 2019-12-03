import System.Environment
data Computer = Computer {
  memory :: [Int]
  , currentInstruction :: Int
  , finished :: Bool
} deriving (Show)

parseProgram :: String -> [Int]
parseProgram str = 
  let spaceSeparated = [if c == ',' then ' ' else c | c <- str]
      in [read i :: Int | i <- words spaceSeparated]

extractOperands :: Computer  -> (Int, Int, Int)
extractOperands (Computer memory insPointer _) = 
    let left:right:target:rest = drop (insPointer + 1) memory
    in (left, right, target)

instruction :: Computer -> (Int -> Int -> Int)  -> Computer
instruction comp operation = 
    let (Computer memory insPointer fin) = comp
        (aPointer, bPointer, targetPointer) = extractOperands comp
        a = memory !! aPointer
        b = memory !! bPointer
        result = operation a b
        (left, rightAndReplaced) = splitAt targetPointer memory
        right = drop 1 rightAndReplaced
        in Computer (left ++ result:right) (insPointer + 4) fin

add :: Int -> Int -> Int
add a b = a + b

mult :: Int -> Int -> Int
mult a b = a * b

executeInstruction :: Computer -> Computer
executeInstruction comp
  | ins == 1 = instruction comp add 
  | ins == 2 = instruction comp mult
  | ins == 99 = Computer memory insPointer True
  where (Computer memory insPointer _) = comp
        ins = memory !! insPointer

executeProgram :: Computer -> Computer
executeProgram comp
  | fin = comp
  | otherwise = executeProgram (executeInstruction comp)
  where fin = finished comp

setNounAndVerb :: Computer -> Int -> Int -> Computer
setNounAndVerb (Computer memory pointer fin) noun verb = 
  let a:surplus = memory
      rest = drop 2 surplus
  in Computer (a:noun:verb:rest) pointer fin

calcResult :: Computer -> Int -> Int -> Int
calcResult comp noun verb = head (memory (executeProgram (setNounAndVerb comp noun verb)))

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let initialState = Computer (parseProgram contents) 0 False
  let results = [noun * 100 + verb | noun <- [1..99],
                                     verb <- [1..99],
                                     calcResult initialState noun verb == 19690720]
  print (head results)
