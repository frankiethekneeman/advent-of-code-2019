import System.Environment
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Space = Free | Bug | Recursion deriving (Show, Eq, Enum)

toSpace :: Char -> Space
toSpace '.' = Free
toSpace '#' = Bug
toSpace '?' = Recursion

toChar :: Space -> Char
toChar Free = '.'
toChar Bug = '#'
toChar Recursion = '?'

type Point = (Int, Int)
type Locus = (Int, Point)
type BugLocii = Set.Set Locus

toStr :: Int -> BugLocii -> [Char]
toStr level locii = unlines $ map (map (toChar.space)) pts
  where space l = if l == (level, (2, 2)) then Recursion else if l `Set.member` locii then Bug else Free
        pts = [[(level, (x, y)) | x <- [0..4]] | y <- [0..4]]

liveOrDie :: Space -> Int -> Space
liveOrDie Bug 1 = Bug
liveOrDie Bug _ = Free
liveOrDie Free 1 = Bug
liveOrDie Free 2 = Bug
liveOrDie Free _ = Free

emitUp :: Locus -> [Locus]
emitUp (l, (_, 0)) = [(l - 1, (2, 1))]
emitUp (l, (2, 3)) = [(l + 1, (x, 4)) | x <- [0..4]]
emitUp (l, (x, y)) = [(l, (x, y - 1))]

emitDown :: Locus -> [Locus]
emitDown (l, (_, 4)) = [(l - 1, (2, 3))]
emitDown (l, (2, 1)) = [(l + 1, (x, 0)) | x <- [0..4]]
emitDown (l, (x, y)) = [(l, (x, y + 1))]

emitLeft :: Locus -> [Locus]
emitLeft (l, (0, _)) = [(l - 1, (1, 2))]
emitLeft (l, (3, 2)) = [(l + 1, (4, y)) | y <- [0..4]]
emitLeft (l, (x, y)) = [(l, (x - 1, y))]

emitRight :: Locus -> [Locus]
emitRight (l, (4, _)) = [(l - 1, (3, 2))]
emitRight (l, (1, 2)) = [(l + 1, (0, y)) | y <- [0..4]]
emitRight (l, (x, y)) = [(l, (x + 1, y))]

emit :: Locus -> [Locus]
emit l = emitUp l ++ emitDown l ++ emitLeft l ++ emitRight l

countEq :: [Locus] -> Map.Map Locus Int
countEq locii = foldl ins Map.empty locii
  where ins m l = let c = Map.findWithDefault 0 l m in Map.insert l (c + 1) m

isBug :: BugLocii -> (Locus, Int) -> Bool
isBug locii (l, ct) = liveOrDie current ct == Bug
  where current = if l `Set.member` locii then Bug else Free

tick :: BugLocii -> BugLocii
tick locii = locii'
  where emissions = concat $ map emit $ Set.elems locii
        emCounts = countEq emissions
        locii' = Set.fromList $ map fst $ filter (isBug locii) $ Map.assocs emCounts

minutes :: Int -> BugLocii -> BugLocii
minutes 0 locii = locii
minutes n locii = minutes (n - 1) $ tick locii
    
main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let scan = map (map toSpace) $ lines contents
  let bugs = Set.fromList [(0, (x, y)) | x <- [0..4], y <-[0..4], scan !! y !! x == Bug]
  let final = minutes 200 bugs
  -- let minLevel = minimum $ map fst $ Set.elems final
  -- let maxLevel = maximum $ map fst $ Set.elems final
  -- let maps = unlines $ intersperse "" $ map (`toStr` final ) [minLevel..maxLevel]
  -- putStrLn maps
  print $ Set.size final

  
