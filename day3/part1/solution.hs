import System.Environment

data Instruction = Instruction {
  direction :: Char
  , distance :: Int
} deriving (Show)

data Point = Point {
  x :: Int
  , y :: Int
} deriving (Show)

data Line = Line {
  orientation :: Char -- H or V
  , along :: Int
  , small :: Int
  , big :: Int
} deriving (Show)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace seek replacement target = 
  [ if x == seek then replacement else x | x <- target ]

parseInstruction :: [Char] -> Instruction
parseInstruction (direction:digits) = 
  let distance = read digits :: Int
    in Instruction direction distance

move :: Point -> Instruction -> Point
move (Point x y) (Instruction 'U' distance) = Point x (y + distance)
move (Point x y) (Instruction 'D' distance) = Point x (y - distance)
move (Point x y) (Instruction 'L' distance) = Point (x - distance) y
move (Point x y) (Instruction 'R' distance) = Point (x + distance) y

makeLine :: Point -> Point -> Line
makeLine (Point x1 y1) (Point x2 y2)
  | x1 == x2 = Line 'V' x1 (min y1 y2) (max y1 y2)
  | y1 == y2 = Line 'H' y1 (min x1 x2) (max x1 x2)

ordered :: Int -> Int -> Int -> Bool
ordered a b c
  | a <= b && b <= c = True
  | otherwise = False

parseWire :: [Char] -> [Line]
parseWire line =
  let pieces = words (replace ',' ' ' line)
      instructions = map parseInstruction pieces
      points = scanl move (Point 0 0) instructions
      pairs = zip points (tail points)
    in map (uncurry makeLine) pairs

intersection :: Line -> Line -> [Point]
intersection first second
  | o1 == o2 && a1 == a2 && s2 < s1 = intersection second first
  | o1 == o2 && a1 == a2 && s2 < b1 && o1 == 'H' = [ Point x a1 | x <- [s2..(min b1 b2)]]
  | o1 == o2 && a1 == a2 && s2 < b1 && o1 == 'V' = [ Point a1 y | y <- [s2..(min b1 b2)]]
  | o1 == 'V' && o2 == 'H' = intersection second first
  | o1 /= o2 && ordered s1 a2 b1 && ordered s2 a1 b2 = [ (Point a2 a1) ]
  | otherwise = []
  where (Line o1 a1 s1 b1) = first
        (Line o2 a2 s2 b2) = second

manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = abs x + abs y

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let (wire1:wire2:_) = map parseWire (lines contents)
  let intersections = concat [ (intersection line1 line2) | line1 <- wire1, line2 <- wire2 ]
  let validIntersections = filter (\(Point x y) -> x /= 0 || y /= 0) intersections
  print (minimum (map manhattanDistance validIntersections))
