import System.Environment
import Data.Array
import Data.Char
import qualified Data.Set as Set
import Data.List
import Debug.Trace

data Tile = Wall | Open | Entrance | Key Char | Door Char deriving (Eq, Show, Ord)

isKey :: Tile -> Bool
isKey (Key _) = True
isKey _ = False

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

passable :: Tile -> Set.Set Tile -> Bool
passable Wall _ = False
passable (Door d) seen = (Key (toLower d)) `Set.member` seen
passable _ _ = True

obtainableKeys :: [Path] -> [Path] -> Set.Set Tile -> Set.Set Point -> Maze -> [Path]
obtainableKeys found [] _ _ _ = reverse found
obtainableKeys found (curr:rest) obtainedKeys seen maze = 
  if isKey currTile && Set.notMember currTile obtainedKeys
    then obtainableKeys (curr:found) rest obtainedKeys seen maze
    else obtainableKeys found rest' obtainedKeys seen' maze
  where (x, y) = head curr
        currTile = maze ! y ! x
        possibleNext =  [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], abs dx + abs dy == 1]
        next = filter ((`passable` obtainedKeys) . (`deref` maze)) $ filter (`Set.notMember` seen) possibleNext
        seen' = foldl (flip Set.insert) seen next
        rest' = rest ++ map (:curr) next

data Route = Route {
  path :: Path
  , keys :: Set.Set Tile
} deriving (Show)

extend :: Route -> Path -> Maze -> Route
extend (Route path obtained) cont maze = 
  let end = head cont
      obtained' = Set.insert (deref end maze) obtained
      path' = cont ++ path
    in Route path' obtained'

merge :: [Route] -> [Route] -> [Route]
merge [] [] = []
merge l [] = l
merge [] r = r
merge (l:lest) (r:rest)
  | length (path l) > length (path r) = r:(merge (l:lest) rest)
  | otherwise = l:(merge lest (r:rest))

paths :: [Route] -> Maze -> Route
paths [] _ = Route [] Set.empty
paths (curr:rest) maze 
  -- | trace (show curr) False = undefined
  | continuations == [] = curr
  | otherwise = paths rest' maze
  where (Route p obtained) = curr
        startPos = head p
        startTile = deref startPos maze
        obtained' = if isKey startTile then Set.insert startTile obtained else obtained
        continuations = map init $ obtainableKeys [] [[ startPos ]] obtained' (Set.singleton startPos) maze
        futures = map (\c -> extend curr c maze) continuations
        rest' = merge rest futures

        
main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  putStrLn contents
  let tiles = map (map toTile) $ lines contents
  let maze = listToZeroArray $ map listToZeroArray tiles
  let startPos = head [(x, y) | x <- indices ( maze ! 0), y <- indices maze, (maze ! y ! x) == Entrance]
  print startPos
  let p = reverse $ path $ paths [(Route [startPos] Set.empty)] maze
  putStrLn $ unlines $ map (\p -> show (p, deref p maze)) p
  print $ (length p) - 1
