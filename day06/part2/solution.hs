import System.Environment
import Data.List
import Control.Applicative
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

hasBody :: [Char] -> Body -> Bool
hasBody target (Body name satellites)
  | target == name = True
  | otherwise = any (hasBody target) satellites

depthTo :: Int -> [Char] -> Body -> Maybe Int
depthTo depth target (Body name satellites)
  | target == name = Just depth
  | otherwise = depthTo (depth + 1) target =<< (find (hasBody target) satellites)

calculateAssistsHelper :: Body -> Body -> Maybe Int
calculateAssistsHelper santa me
  | (name santa) == (name me) = calculateAssists santa
  | otherwise = (+) <$> (depthTo 0 "SAN" santa) <*> (depthTo 0 "YOU" me)

calculateAssists :: Body -> Maybe Int
calculateAssists (Body _ [sat]) = calculateAssists sat
calculateAssists (Body _ satellites) =
  let santa = find (hasBody "SAN") satellites
      me = find (hasBody "YOU") satellites
      assists = calculateAssistsHelper <$> santa <*> me
    in case assists of
        Just m -> m
        otherwise -> Nothing


main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let orbits = sortBy (comparator [body, satellite]) (map orbit (lines contents))
  let registry = (Map.fromAscList) (groupByBody orbits)
  let tree = toBody "COM" registry
  case calculateAssists tree of 
    Just assists -> print assists
    otherwise -> print "Failed to calculate Assists."
