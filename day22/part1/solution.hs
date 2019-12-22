import System.Environment
import Data.List
import Data.Array
import Debug.Trace

data Instruction = NewStack | Cut Int | Increment Int deriving (Show)

type Shuffle = [Int] -> [Int]

newStack :: Shuffle
newStack = reverse

(%) :: Int -> Int -> Int
(%) a b
  | a < 0 = ((a `mod` b) + b) `mod` b
  | otherwise = a `mod` b

cut :: Int -> Shuffle
cut d list = end ++ begin
  where cutSize = d % (length list)
        (begin, end) = splitAt cutSize list

dealWithIncrement :: Int -> Shuffle
dealWithIncrement inc list = elems arr
  where len = length list
        indices = map (\i -> (i * inc) % len) [0..]
        targeted = zip indices list
        arr = array (0, len -1 ) targeted

parseInstruction :: [Char] -> Instruction
parseInstruction "deal into new stack" = NewStack
parseInstruction line
  | "cut " `isPrefixOf` line = Cut $ read $ drop 4 line
  | otherwise = Increment $ read $ drop 20 line

shuffle :: Instruction -> Shuffle
shuffle NewStack = newStack
shuffle (Cut d) = cut d
shuffle (Increment i) = dealWithIncrement i

deckSize :: Int
deckSize = 10007

factoryOrderDeck :: [Int]
factoryOrderDeck = [0..(deckSize - 1)]

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let instructions = map parseInstruction $ lines contents
  let result = foldl (flip shuffle) factoryOrderDeck instructions
  -- print instructions
  case elemIndex 2019 result of
    Just x -> print x
    Nothing -> putStrLn "No card 2019"
