import System.Environment
import Data.List
import qualified Data.Set as Set

comparing :: (Ord b) => [a -> b] -> a -> a -> Ordering
comparing [f] l r = compare (f l) (f r)
comparing (f:fs) l r = 
  let result = compare (f l) (f r)
  in if result == EQ then comparing fs l r else result

equating :: (Eq b) => [a -> b] -> a -> a -> Bool
equating fs l r = all (\f -> f l == f r) fs

data Recipe = Recipe {
  reactants :: [(Int, [Char])]
  , result :: [Char]
  , yield :: Int
} deriving (Show)

splitStr :: [Char] -> [Char] -> ([Char], [Char])
splitStr search target
  | not (search `isInfixOf` target) = ([], target)
  | otherwise = 
    let tok = head search
        l = length search
        (begin, end) = span (/=tok) target
      in if take l end == search
        then (begin, drop l end)
        else let (part2, rest) = splitStr search $ tail end
               in (begin ++ [tok] ++ part2, rest)

splitOn :: [Char] -> [Char] -> [[Char]]
splitOn token string =
  let (first, rest) = splitStr token string
    in if first == []
      then [rest]
      else first:(splitOn token rest)

parseIngredient :: [Char] -> (Int, [Char])
parseIngredient s = 
  let (n, chem) = span (/=' ') s
    in (read n :: Int, tail chem)

parseRecipe :: [Char] -> Recipe
parseRecipe line =
  let [lhs, rhs] = splitOn " => " line
      reactants = map parseIngredient $ splitOn ", " lhs
      (yield, result) = parseIngredient rhs
    in Recipe reactants result yield

canMake :: Set.Set [Char] -> Recipe -> Bool
canMake ingredients Recipe{reactants = reactants} =
  all ((flip Set.member $ ingredients).snd) reactants

creationSort :: Set.Set [Char] -> [Recipe] -> [Recipe]
creationSort _ [] = []
creationSort known recipes = 
  let (makeable, unmakeable) = partition (canMake known) recipes
      newKnown = Set.fromList $ map result makeable
      in makeable ++ creationSort (Set.union known newKnown) unmakeable

unapply :: Recipe -> (Int, [Char]) -> [(Int, [Char])]
unapply (Recipe reactants target yield) (needed, chemical)
  | target /= chemical = [(needed, chemical)]
  | otherwise = 
    let q = needed `quot` yield
        m = if needed `mod` yield == 0 then q else q + 1
      in map (\(count, ing) -> (count * m, ing)) reactants

unapplyAll :: Recipe -> [(Int, [Char])] -> [(Int, [Char])]
unapplyAll recipe quantities =
  let expanded = concat $ map (unapply recipe) quantities
      grouped = groupBy (equating [snd]) $ sortBy (comparing [snd]) expanded
      flat = map (foldl1 (\(l, name) (r, _) -> (l + r, name))) grouped
    in flat

oreNeeded :: Int -> [Recipe] -> Int
oreNeeded n expansions =
  fst.head $ foldl (flip unapplyAll) [(n, "FUEL")] expansions

oreInHold = 1000000000000
maxOre :: Int -> Int -> [Recipe] -> Int
maxOre l r expansions
  | l + 1 == r = l
  | otherwise =
    let candidate = (l + r) `quot` 2
        ore = oreNeeded candidate expansions
      in if ore > oreInHold
        then maxOre l candidate expansions
        else maxOre candidate r expansions

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let recipes = map parseRecipe $ lines contents
  let expansions = reverse $ creationSort (Set.singleton "ORE") recipes
  print $ maxOre 1 oreInHold expansions
