import System.Environment

replace :: (Eq a) => a -> a -> [a] -> [a]
replace seek replacement target = 
  [ if x == seek then replacement else x | x <- target ]

getMinMax :: [Char] -> (Int, Int)
getMinMax line = 
  let split = words (replace '-' ' ' line)
      min = read (head split) :: Int
      max = read ((head . tail) split) :: Int
      in (min, max)

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates [] = False
hasDuplicates [_] = False
hasDuplicates (first:second:rest) 
  | first == second = True
  | otherwise = hasDuplicates (second:rest)

nonDecreasing :: (Ord a) => [a] -> Bool
nonDecreasing [] = True
nonDecreasing [_] = True
nonDecreasing (first:second:rest) 
  | first > second = False
  | otherwise = nonDecreasing (second:rest)

isLength6 :: [a] -> Bool
isLength6 list = 6 == length list

splitDuplicates :: (Eq a) => [a] -> [[a]]
splitDuplicates [] = []
splitDuplicates list = 
  let first = head list
      lead = takeWhile (==first) list
      tail = dropWhile (==first) list
    in lead:(splitDuplicates tail)

hasIsolatedDouble :: (Eq a) => [a] -> Bool
hasIsolatedDouble list = 
  let contiguousBlocks = splitDuplicates list
      lengths = map length contiguousBlocks
    in elem 2 lengths

validPassword :: Int -> Bool
validPassword candidate =
  let asStr = show candidate
      rules = [isLength6, hasDuplicates, nonDecreasing, hasIsolatedDouble]
      results = map (\ f -> f asStr) rules
    in foldr (&&) True results

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let (min, max) = getMinMax (head (lines contents))
  let passwords = [ x | x <-[min..max], validPassword x ]
  print (length passwords)
