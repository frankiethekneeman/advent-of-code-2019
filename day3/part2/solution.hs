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
  , start :: Int
  , finish :: Int
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
  | x1 == x2 = Line 'V' x1 y1 y2
  | y1 == y2 = Line 'H' y1 x1 x2

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
  where (Line o1 a1 start1 fin1) = first
        (Line o2 a2 start2 fin2) = second
        s1 = min start1 fin1
        s2 = min start2 fin2
        b1 = max start1 fin1
        b2 = max start2 fin2

isOn :: Point -> Line -> Bool
isOn (Point x y) (Line orientation along start finish)
  | orientation == 'H' = y == along && ordered mn x mx
  | orientation == 'V' = x == along && ordered mn y mx
  where mn = min start finish
        mx = max start finish

distAlong :: Line -> Point -> Int
distAlong (Line 'H' _ start _) (Point x _) = abs (x - start)
distAlong (Line 'V' _ start _) (Point _ y) = abs (y - start)

len :: Line -> Int
len (Line _ _ start finish) = abs (finish - start)

wireDistance :: [Line] -> Point -> Int
wireDistance line point = sum (wireDistanceSteps line point)

wireDistanceSteps :: [Line] -> Point -> [Int]
wireDistanceSteps (l:rest) p
  | p `isOn` l = [distAlong l p]
  | otherwise = (len l):(wireDistanceSteps rest p)

combinedWireDistance :: [Line] -> [Line] -> Point -> Int
combinedWireDistance w1 w2 p = wireDistance w1 p + wireDistance w2 p

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let (wire1:wire2:_) = map parseWire (lines contents)
  let intersections = concat [ (intersection line1 line2) | line1 <- wire1, line2 <- wire2 ]
  let validIntersections = filter (\(Point x y) -> x /= 0 || y /= 0) intersections
  print (minimum (map (combinedWireDistance wire1 wire2) validIntersections))
