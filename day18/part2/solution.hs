import System.Environment
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace

data Tile = Wall | Open | Entrance | Key Char | Door Char | UL | UR | LL |LR deriving (Eq, Show, Ord)

isKey :: Tile -> Bool
isKey (Key _) = True
isKey _ = False

isDoor :: Tile -> Bool
isDoor (Door _) = True
isDoor _ = False

keyOf :: Tile -> Tile
keyOf (Door d) = Key (toLower d)

toTile :: Char -> Tile
toTile '#' = Wall
toTile '.' = Open
toTile '@' = Entrance
toTile x
  | 'a' <= x && x <= 'z' = Key x
  | 'A' <= x && x <= 'Z' = Door x

listToZeroArray :: [a] -> Array Int a
listToZeroArray l = listArray (0, (length l)-1) l

type Maze = Array Int (Array Int Tile)
type Point = (Int, Int)
type Path = [Point]

deref :: Point -> Maze -> Tile
deref (x, y) m 
  | x < minX || x > maxX || y < minY || y > maxY = Wall
  | otherwise = m ! y ! x
  where (minX, maxX) = bounds $ m ! 0
        (minY, maxY) = bounds $ m

passable :: Tile -> Bool
passable Wall = False
passable _ = True

bfs :: Point -> [Path] -> Set.Set Point -> Maze -> Maybe Path
bfs t [] _ _ = Nothing
bfs t ((curr:prev):others) seen maze
  | t == curr = Just (curr:prev)
  | otherwise = bfs t others' seen' maze
  where (x, y) = curr
        possibleNext =  [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], abs dx + abs dy == 1]
        next = filter (passable . (`deref` maze)) $ filter (`Set.notMember` seen) possibleNext
        seen' = foldl (flip Set.insert) seen next
        others' = others ++ map (:curr:prev) next

data Segment = Segment {
   len :: Int
   , keysNeeded :: Set.Set Tile
   , keysAlong :: Set.Set Tile
} deriving (Show, Eq)

findSegment :: Point -> Point -> Maze -> Maybe Segment
findSegment start end maze = case bfs end [[start]] (Set.singleton start) maze of
  Nothing -> Nothing
  Just p ->
    let path = tail $ reverse $ map (`deref` maze) $ p
        keysNeeded = map keyOf $ filter isDoor path
        keysAlong = filter isKey $ path
        keysAndDoors = zip [0..] $ filter (\t -> isKey t || isDoor t) path
        keysPickedUpThenUsed = [ key
          | (k, key) <- keysAndDoors
          , (d, door) <- keysAndDoors
          , k < d
          , isDoor door
          , key == keyOf door ]
        keysNeeded' = filter (not.(`elem` keysPickedUpThenUsed)) keysNeeded
      in Just (Segment (length path) (Set.fromList keysNeeded') (Set.fromList keysAlong))

interesting :: Tile -> Bool
interesting Wall = False
interesting (Door _) = False
interesting Open = False
interesting _ = True

type Graph = Map.Map Tile (Map.Map Tile Segment)

type Position = Array Int Tile
startPos :: Position
startPos = listArray (0,3) [UL, UR, LL, LR]
data Route = Route {
    l :: Int
    , keysObtained :: Set.Set Tile
    , position :: Position
} deriving (Show)

canTravel :: Set.Set Tile -> Segment -> Bool
canTravel keysObtained (Segment _ keysNeeded _ ) = keysNeeded `Set.isSubsetOf` keysObtained

withoutKeys :: Ord a => Map.Map a b -> Set.Set a -> Map.Map a b
withoutKeys map set = Map.filterWithKey (\k _ -> k `Set.notMember` set) map

asRoute :: Set.Set Tile -> (Position, Segment) -> Route
asRoute seen (pos, (Segment len keysNeeded keysAlong)) = 
  Route len (Set.union seen keysAlong) pos

data Instruction = Instruction {
  robot :: Int
  , cost :: Int
  , keys :: Set.Set Tile
  , end :: Tile
} deriving (Show)

asInstruction :: Int -> Set.Set Tile -> (Tile, Segment) -> Instruction
asInstruction robot seen (pos, (Segment len keysNeeded keysAlong)) = 
  Instruction robot len (Set.union seen keysAlong) pos

getNext :: Int -> Graph -> Set.Set Tile -> Tile -> [Instruction]
getNext robot graph seen curr = 
  let possible = (graph Map.! curr) `withoutKeys` seen
      visible = Map.filter (canTravel seen) possible
    in map (asInstruction robot seen) $ Map.assocs visible

apply :: Set.Set Tile -> Position -> Instruction -> Route
apply seen pos (Instruction robot cost keys end) =
  Route cost (Set.union seen keys) (pos // [(robot, end)])

getAllNext :: Graph -> Set.Set Tile -> Position -> [Route]
getAllNext graph seen pos =
  let instructions = concat $ map (\(r, t) -> getNext r graph seen t) $ assocs pos
    in map (apply seen pos) instructions

type Cache = Map.Map (Set.Set Tile, Position) Int

doLookup :: Graph -> Set.Set Tile -> Position -> Cache -> (Cache, Int)
doLookup graph keys pos cache = case Map.lookup (keys, pos) cache of
  Just d -> (cache, d)
  Nothing -> calculate graph keys pos cache
    
foldBehaviour :: Graph -> (Cache, Int) -> Route -> (Cache, Int)
foldBehaviour graph (cache, prevBest) (Route l keys pos) = 
  let (cache', d) = doLookup graph keys pos cache
      new = l + d
      best = if prevBest == -1 then new else min prevBest new
    in (cache', best)

calculate :: Graph -> Set.Set Tile -> Position -> Cache -> (Cache, Int)
calculate graph keys pos cache =
  let possible = (Map.keysSet graph) Set.\\ (Set.fromList [UL, UR, LL, LR, Entrance])
      next = getAllNext graph keys pos
      (cache', d) = foldl (foldBehaviour graph) (cache, -1) next
      cache'' = Map.insert (keys, pos) d cache'
    in if keys == possible
      then (cache, 0)
      else (cache'', d) -- Why do I have to take a max here?

updates :: [(Int, Int, Tile)]
updates = [
  (-1, -1, UL),
  (-1,  0, Wall),
  (-1,  1, LL),
  ( 0, -1, Wall),
  ( 0,  0, Wall),
  ( 0,  1, Wall),
  ( 1, -1, UR),
  ( 1,  0, Wall),
  ( 1,  1, LR) ]

update :: Maze -> Int -> Int -> Tile -> Maze
update maze x y tile = maze // [(y, (maze ! y) // [(x, tile)])]

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let tiles = map (map toTile) $ lines contents
  let rawMaze = listToZeroArray $ map listToZeroArray tiles
  let allPoints = [(x, y) | x <- indices (rawMaze ! 0), y <- indices rawMaze]
  let (eX, eY) = head $ filter ((==Entrance).(`deref` rawMaze)) allPoints
  let maze = foldl (\m (dx, dy, t) -> update m (eX + dx) (eY + dy) t) rawMaze updates
  let poi = [p | p <- allPoints, interesting $ deref p maze]

  let graph = Map.fromList [ (
                  deref from maze,
                  Map.fromList [(deref to maze, fromJust mSeg)
                    | to <- poi
                    , to /= from
                    , isKey (deref to maze)
                    , let mSeg = findSegment from to maze
                    , mSeg /= Nothing
                    ]
               )
               | from <- poi
             ]
  print $ snd $ doLookup graph Set.empty startPos Map.empty
