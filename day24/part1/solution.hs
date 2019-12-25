import System.Environment
import Data.List
import Data.Array
import qualified Data.Set as Set

powersOfTwo :: [Int]
powersOfTwo = map (2^) [0..]

listToZeroArray :: [a] -> Array Int a
listToZeroArray l = listArray (0, (length l)-1) l

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf len list
  | length list `mod` len == 0 = (take len list):(chunksOf len $ drop len list)

size :: Array Int a -> Int
size m = (maxI - minI) + 1
  where (minI, maxI) = bounds m

data Space = Free | Bug deriving (Show, Eq, Enum)

toSpace :: Char -> Space
toSpace '.' = Free
toSpace '#' = Bug

toChar :: Space -> Char
toChar Free = '.'
toChar Bug = '#'

type ErisMap = Array Int (Array Int Space)

flatten :: ErisMap -> [Space]
flatten m = concat $ (map elems) $ elems m

unFlatten :: Int -> [Space] -> ErisMap
unFlatten rowL spaces = listToZeroArray $ map listToZeroArray chunks
  where chunks = chunksOf rowL spaces

height :: ErisMap -> Int
height m = size m

width :: ErisMap -> Int
width m = case nub $ map size $ elems m of
  [w] -> w

toStr :: ErisMap -> [Char]
toStr m = unlines $ map ((map toChar).elems) $ elems m

type Point = (Int, Int)

cardinals :: Point -> [Point]
cardinals (x, y) = [(x + dx, y + dy)
                     | dx <- [-1..1]
                     , dy <- [-1..1]
                     , abs dx + abs dy == 1
                   ]

(%) :: ErisMap -> Point -> Space
(%) m (x, y) = m ! y ! x

(?) :: ErisMap-> Point -> Bool
(?) m (x, y) =
  let (minY, maxY) = bounds m
      (minX, maxX) = bounds (m ! y)
    in y >= minY && y <= maxY && x >= minX && x <= maxX

points :: ErisMap -> [Point]
points m = [ (x, y) | y <- indices m, x <- indices (m ! y)]

liveOrDie :: Space -> Int -> Space
liveOrDie Bug 1 = Bug
liveOrDie Bug _ = Free
liveOrDie Free 1 = Bug
liveOrDie Free 2 = Bug
liveOrDie Free _ = Free

calculate :: Point -> ErisMap -> Space
calculate p m = liveOrDie self bugNeighbors
  where self = m % p
        bugNeighbors = length [n | n <- cardinals p, m ? n, m % n == Bug]

tick :: ErisMap -> ErisMap
tick before = unFlatten (width before) calculated
  where calculated = map (`calculate` before) (points before)

score :: ErisMap -> Int
score m = sum $ zipWith (*) powersOfTwo $ map fromEnum $ flatten m

scoreProgression :: ErisMap -> [Int]
scoreProgression m = (score m):(scoreProgression $ tick m)

findFirstDuplicate :: Ord a => [a] -> Set.Set a -> a
findFirstDuplicate (x:xs) seen
  | x `Set.member` seen = x
  | otherwise = findFirstDuplicate xs (Set.insert x seen)
    
main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let bugs = map (map toSpace) $ lines contents
  let eris = listToZeroArray $ map listToZeroArray bugs
  putStrLn $ toStr eris
  print $ findFirstDuplicate (scoreProgression eris) Set.empty
