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

orbit :: [Char] -> ([Char], [Char])
orbit str = 
  let [body, satellite] = words (replace ')' ' ' str)
    in (satellite, body)

distBetween :: Map.Map [Char] [Char] -> [Char] -> [Char] -> Maybe Int
distBetween registry from to 
  | from == to = Just 0
  | otherwise = fmap (+1) $ distBetween registry from =<< Map.lookup to registry

sumMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
sumMaybe a b = (+) <$> a <*> b

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let orbits = map orbit $ lines contents
  let registry = (Map.fromList) orbits
  case (foldl1 sumMaybe $ map (distBetween registry "COM") $ Map.keys registry) of
    Just x -> print x
    Nothing -> print "Mistake."
