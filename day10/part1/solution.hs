import System.Environment
import Data.List

data Asteroid = Asteroid {
    x :: Int
    , y :: Int
} deriving (Show, Eq)

data Slope = Slope {
  dx :: Int
  , dy :: Int
} deriving (Eq, Show)

data Grid = Up | Down | Leftward | Rightward deriving (Eq, Show)
type Angle = Either Grid Slope

angle :: Asteroid -> Asteroid -> Angle
angle (Asteroid x1 y1) (Asteroid x2 y2)
  | x1 == x2 && y1 > y2 = Left Down
  | x1 == x2 && y1 < y2 = Left Up
  | y1 == y2 && x1 < x2 = Left Rightward
  | y1 == y2 && x1 > x2 = Left Leftward
  | x1 /= x2 && y1 /= y2 = 
    let dx = x2 - x1
        dy = y2 - y1 
        factor = gcd dx dy
      in Right $ Slope (dx `quot` factor) (dy `quot` factor)

calcAngles :: [Asteroid] -> [(Asteroid, Int)]
calcAngles asteroids = [ (a, length uniq) |
                            a <- asteroids
                            , let notA =  filter (/=a) asteroids
                            , let angles = map (angle a) notA
                            , let uniq = nub angles ]

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
  print $ maximum $ map snd $ calcAngles $ extractAsteroids contents
