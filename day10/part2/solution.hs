import System.Environment
import Data.List

comparing :: (Ord b) => [a -> b] -> a -> a -> Ordering
comparing [f] l r = compare (f l) (f r)
comparing (f:fs) l r = 
  let result = compare (f l) (f r)
  in if result == EQ then comparing fs l r else result

equating :: (Eq b) => [a -> b] -> a -> a -> Bool
equating fs l r = all (\f -> f l == f r) fs

data Asteroid = Asteroid {
    x :: Int
    , y :: Int
} deriving (Show, Eq)

data Slope = Slope {
  dx :: Int
  , dy :: Int
} deriving (Eq, Show, Ord)

data Grid = Up | Rightward | Down | Leftward deriving (Eq, Show, Ord, Enum)
data Angle = Diagonal Slope | Cardinal Grid deriving (Eq, Show, Ord)

angle :: Asteroid -> Asteroid -> Angle
angle (Asteroid x1 y1) (Asteroid x2 y2)
  | x1 == x2 && y1 > y2 = Cardinal Up
  | x1 == x2 && y1 < y2 = Cardinal Down
  | y1 == y2 && x1 < x2 = Cardinal Rightward
  | y1 == y2 && x1 > x2 = Cardinal Leftward
  | x1 /= x2 && y1 /= y2 = 
    let dx = x2 - x1
        dy = y1 - y2 -- The numbering is backwards for y 
        factor = gcd dx dy
      in Diagonal $ Slope (dx `quot` factor) (dy `quot` factor)
dist :: Asteroid -> Asteroid -> Int
dist (Asteroid x1 y1) (Asteroid x2 y2) = (abs (x2 - x1)) + (abs (y2 - y1))

toRadians :: Angle -> Float
toRadians (Cardinal x) = (fromIntegral $ fromEnum x) * pi / 2 
toRadians (Diagonal (Slope dx dy)) = 
  let raw = atan2 (fromIntegral dy) (fromIntegral dx)
      offset = (pi / 2) - raw
    in if offset < 0 then (2 * pi) + offset else offset

countVisible :: [Asteroid] -> [(Asteroid, Int)]
countVisible asteroids = [ (a, length uniq) |
                            a <- asteroids
                            , let notA =  filter (/=a) asteroids
                            , let angles = map (angle a) notA
                            , let uniq = nub angles ]

unroll :: (Eq a) => [[a]] -> [a]
unroll [] = []
unroll lists = 
  let firsts = map head lists
      remaining = filter (/=[]) $ map tail lists
    in firsts ++ unroll remaining

destructionOrder :: Asteroid -> [Asteroid] -> [Asteroid]
destructionOrder station asteroids =
  let notA = filter (/=station) asteroids
      same = groupBy (equating [angle station]) $ sortBy (comparing [angle station]) notA
      radianSort = sortBy (comparing [toRadians.(angle station).head]) same
      subSorted = map (sortBy (comparing [dist station])) radianSort
    in unroll subSorted

extractAsteroids :: [Char] -> [Asteroid]
extractAsteroids input = 
  let enumerated = map (\(y, row) -> (y, zip [0..] row)) $ zip [0..] $ lines input :: [(Int, [(Int, Char)])]
      flattened = concat $ map (\(y, row) -> map (\(x, c) -> ((x, y), c)) row) enumerated :: [((Int, Int), Char)]
      filtered = filter ((=='#').snd) flattened
    in map (\((x,y), '#') -> Asteroid x y) filtered

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let asteroids = extractAsteroids contents

  let station = fst $ maximumBy (comparing [snd]) $ countVisible asteroids
  let target = destructionOrder station asteroids !! 199 --zero based indexing
  
  print $ (x target) * 100 + (y target)
