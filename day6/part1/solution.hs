import System.Environment
import Data.List
import qualified Data.Map.Strict as Map

replace :: (Eq a) => a -> a -> [a] -> [a]
replace seek replacement target = 
  [ if x == seek then replacement else x | x <- target ]

comparator :: (Ord b) => [(a -> b)] -> a -> a -> Ordering
comparator [] l r = EQ
comparator (f:rest) l r
  | result == EQ = comparator rest l r
  | otherwise = result
  where result = compare (f l) (f r)

data Orbit = Orbit {
  body :: [Char]
  , satellite :: [Char]
} deriving (Show)

orbit :: [Char] -> Orbit
orbit str = 
  let [body, satellite] = words (replace ')' ' ' str)
    in Orbit body satellite


groupByBody :: [Orbit] -> [([Char], [[Char]])]
groupByBody [] = []
groupByBody list = 
  let b = (body.head) list
      identifier = (==b).body
      sameBody = takeWhile identifier list
      rest = dropWhile identifier list
      grouped = (b, map satellite sameBody)
    in grouped:(groupByBody rest)

data Body = Body {
  name :: [Char]
  , satellites :: [Body]
} deriving (Show)

toBody :: [Char] -> Map.Map [Char] [[Char]] -> Body
toBody name registry = 
  let satNames = Map.findWithDefault [] name registry
      satellites = map ((flip toBody) registry) satNames
    in Body name satellites

orbitalCheckSum :: Int -> Body -> Int
orbitalCheckSum depth body = depth + sum (map (orbitalCheckSum (depth + 1)) (satellites body))

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let orbits = sortBy (comparator [body, satellite]) (map orbit (lines contents))
  let registry = (Map.fromAscList) (groupByBody orbits)
  let tree = toBody "COM" registry
  print (orbitalCheckSum 0 tree)
