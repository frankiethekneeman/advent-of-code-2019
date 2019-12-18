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

fullOrbit :: [Char] -> Map.Map [Char] [Char] -> Maybe [[Char]]
fullOrbit from registry
  | from == "COM" = Just [from]
  | otherwise =
    let maybeNext = Map.lookup from registry
      in case maybeNext of
        Just next -> (from:) <$> fullOrbit next registry
        Nothing -> Nothing

mergePaths :: [[Char]] -> [[Char]] -> [[Char]]
mergePaths p1 p2
  | l1 < maxLen = mergePaths (p1 ++ (replicate (maxLen - l1) "")) p2
  | l2 < maxLen = mergePaths p1 $ p2 ++ (replicate (maxLen - l2) "")
  | otherwise = 
    let zipped = zip p1 p2
        root = fst.last $ takeWhile (uncurry (==)) zipped
        rest = dropWhile (uncurry (==)) zipped
        rest1 = filter ((>0).length) $ map fst rest
        rest2 = filter ((>0).length) $ map snd rest
      in (reverse rest1) ++ [root] ++ rest2
  where l1 = length p1
        l2 = length p2
        maxLen = max (length p1) (length p2)

pathBetween :: [Char] -> [Char] -> Map.Map [Char] [Char] -> Maybe [[Char]]
pathBetween from to registry =
  let fromOrbit = reverse <$> fullOrbit from registry
      toOrbit = reverse <$> fullOrbit to registry
    in mergePaths <$> fromOrbit <*> toOrbit

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let orbits = map orbit $ lines contents
  let registry = (Map.fromList) orbits
  let path = pathBetween "YOU" "SAN" registry
  let dist = fmap (-3 + ) $ length <$> path
  case dist of 
    Just d -> print d
    Nothing -> print "Failed to calculate Assists."
