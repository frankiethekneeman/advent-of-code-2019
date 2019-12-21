import System.Environment
import Data.Array
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace

data Tile = Wall | Open | Entrance | Key Char | Door Char deriving (Eq, Show, Ord)

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

bfs :: Point -> [Path] -> Set.Set Point -> Maze -> Path
bfs t ((curr:prev):others) seen maze
  | t == curr = curr:prev
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
} deriving (Show)

findSegment :: Point -> Point -> Maze -> Segment
findSegment start end maze = 
  let path = tail $ reverse $ map (`deref` maze) $ bfs end [[start]] (Set.singleton start) maze
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
    in Segment (length path) (Set.fromList keysNeeded') (Set.fromList keysAlong)

interesting :: Tile -> Bool
interesting Entrance = True
interesting (Key _) = True
interesting _ = False

type Graph = Map.Map Tile (Map.Map Tile Segment)

data Route = Route {
    l :: Int
    , keysObtained :: Set.Set Tile
    , position :: Tile
} deriving (Show)

canTravel :: Set.Set Tile -> Segment -> Bool
canTravel keysObtained (Segment _ keysNeeded _ ) = keysNeeded `Set.isSubsetOf` keysObtained

withoutKeys :: Ord a => Map.Map a b -> Set.Set a -> Map.Map a b
withoutKeys map set = Map.filterWithKey (\k _ -> k `Set.notMember` set) map

asRoute :: Set.Set Tile -> (Tile, Segment) -> Route
asRoute seen (pos, (Segment len keysNeeded keysAlong)) = 
  Route len (Set.union seen keysAlong) pos

getNext :: Graph -> Set.Set Tile -> Tile -> [Route]
getNext graph seen curr = 
  let possible = (graph Map.! curr) `withoutKeys` seen
      visible = Map.filter (canTravel seen) possible
    in map (asRoute seen) $ Map.assocs visible

type Cache = Map.Map (Set.Set Tile, Tile) Int

doLookup :: Graph -> Set.Set Tile -> Tile -> Cache -> (Cache, Int)
doLookup graph keys pos cache = case Map.lookup (keys, pos) cache of
  Just d -> (cache, d)
  Nothing -> calculate graph keys pos cache
    
foldBehaviour :: Graph -> (Cache, Int) -> Route -> (Cache, Int)
foldBehaviour graph (cache, prevBest) (Route l keys pos) = 
  let (cache', d) = doLookup graph keys pos cache
      new = l + d
      best = if prevBest == -1 then new else min prevBest new
    in (cache', best)

calculate :: Graph -> Set.Set Tile -> Tile -> Cache -> (Cache, Int)
calculate graph keys pos cache =
  let possible = Set.delete Entrance $ Map.keysSet graph
      next = getNext graph keys pos
      (cache', d) = foldl (foldBehaviour graph) (cache, -1) next
      cache'' = Map.insert (keys, pos) d cache'
    in if keys == possible
      then (cache, 0)
      else (cache'', d)

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let tiles = map (map toTile) $ lines contents
  let maze = listToZeroArray $ map listToZeroArray tiles
  let points = [(x, y) | x <- indices ( maze ! 0), y <- indices maze, interesting (maze ! y ! x)]
  let graph = Map.fromList [ (
                  deref from maze,
                  Map.fromList [(deref to maze, findSegment from to maze) | to <- points, to /=from, (deref to maze) /=Entrance]
               )
               | from <- points
             ]
  print $ snd $ doLookup graph Set.empty Entrance Map.empty
