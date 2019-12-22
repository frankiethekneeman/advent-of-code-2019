import System.Environment
import Data.Array
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace

listToZeroArray :: [a] -> Array Int a
listToZeroArray l = listArray (0, (length l)-1) l

type Point = (Int, Int)

combine :: Point -> Point -> Point
combine (x, y) (dx, dy) = (x + dx, y + dy)

data Direction = U | D | L | R deriving (Show, Eq)

move :: Direction -> Point -> Point
move U = combine ( 0, -1)
move L = combine (-1,  0)
move D = combine ( 0,  1)
move R = combine ( 1,  0)

neighbors :: Point -> [Point]
neighbors p = map (`move` p) [U, L, D, R]

type Maze a = Array Int (Array Int a)

coordinates :: Maze a -> [Point]
coordinates m =
  let genX y row = map (\x -> (x, y)) $ indices row
    in concat $ map (uncurry genX) $ assocs m

deref :: Point -> Maze a -> a
deref (x, y) m = m ! y ! x

(%) :: Maze a -> Point -> a
(%) m p = deref p m

(?) :: Maze a -> Point -> Bool
(?) m (x, y) =
  let (minY, maxY) = bounds m
      (minX, maxX) = bounds (m ! y)
    in y >= minY && y <= maxY && x >= minX && x <= maxX

data Label = Label {
  label :: [Char]
  , neighbor :: Point
} deriving (Show, Eq, Ord)

canPass :: Point -> Maze Char -> Bool
canPass p m = m ? p && m % p == '.'

isLabelChar :: Point -> Maze Char -> Bool
isLabelChar p m = let c = m % p in m ? p && c >= 'A' && c <= 'Z' 

isLabel :: Point -> Maze Char -> Bool
isLabel p m
  | not $ isLabelChar p m = False
  | isLabelChar u m && canPass d m = True
  | isLabelChar d m && canPass u m = True
  | isLabelChar l m && canPass r m = True
  | isLabelChar r m && canPass l m = True
  | otherwise = False
  where [u, l, d, r] = neighbors p

parseLabel :: Point -> Maze Char -> Label
parseLabel p m
  | isLabelChar u m && canPass d m = Label [m % u, m % p] d
  | isLabelChar d m && canPass u m = Label [m % p, m % d] u
  | isLabelChar l m && canPass r m = Label [m % l, m % p] r
  | isLabelChar r m && canPass l m = Label [m % p, m % r] l
  where [u, l, d, r] = neighbors p

data Portal = Entrance Point | BiDirectional Point Point | Exit Point deriving (Show, Eq, Ord)

toPortals :: [Label] -> [Portal]
toPortals [] = []
toPortals ((Label "AA" p):rest) = (Entrance p):(toPortals rest)
toPortals ((Label "ZZ" p):rest) = (Exit p):(toPortals rest)
toPortals ((Label ll lp):(Label rl rp):rest)
  | ll == rl = (BiDirectional lp rp):(toPortals rest)

adjacent :: Point -> Maze Char ->  Set.Set Point
adjacent p maze = Set.fromList $ filter (`canPass` maze) $ neighbors p 

type Adjacency a = Map.Map a (Set.Set a)

buildAdjacencyMap :: Ord a => (a -> [a]) -> (a -> Bool) -> Set.Set a -> Adjacency a
buildAdjacencyMap gen verify keys = Map.fromSet (Set.fromList.filter verify.gen) keys

add :: Ord a => Adjacency a -> a -> a -> Adjacency a
add adj l r = case Map.lookup l adj of
  Nothing -> Map.insert l (Set.singleton r) adj
  Just s -> Map.insert l (Set.insert r s) adj

updateAdjacencyMap :: Adjacency Point -> Portal -> Adjacency Point
updateAdjacencyMap adj (Entrance _) = adj
updateAdjacencyMap adj (Exit _) = adj
updateAdjacencyMap adj (BiDirectional l r) = add (add adj l r) r l

bfs :: Eq a => Ord a => a -> [[a]] -> Adjacency a -> Set.Set a -> [a]
bfs target (path:rest) adj seen
  | curr == target = path
  | otherwise = bfs target rest' adj seen'
  where curr = head path
        next = adj Map.! curr
        seen' = Set.union seen next
        new = map (:path) $ filter (`Set.notMember` seen) $ Set.toList next
        rest' = rest ++ new

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let chars = lines contents
  let maze = listToZeroArray $ map listToZeroArray chars
  let points = coordinates maze
  let passable = Set.fromList $ filter (`canPass` maze) points
  let baseAdj = buildAdjacencyMap neighbors (`canPass` maze) passable
  let portals = sort $ toPortals $ sort $ map (`parseLabel` maze) $ filter (`isLabel` maze) points
  let adj = foldl updateAdjacencyMap baseAdj portals
  let Entrance entrance = head portals
  let Exit exit = last portals
  print $ (length $ bfs exit [[entrance]] adj (Set.singleton entrance)) - 1
