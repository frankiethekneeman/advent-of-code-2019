import System.Environment
import Data.List

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

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let recipes = map parseRecipe $ lines contents
  let results = (length.nub.sort.map result) recipes
  putStrLn $ unlines $ map show recipes
  print (length recipes, results)
