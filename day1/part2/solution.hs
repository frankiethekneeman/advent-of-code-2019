import System.Environment

parseWeights :: String -> [Int]
parseWeights str = 
    [ read i :: Int | i <- lines str ]

calcFuel :: Int -> Int
calcFuel weight
  | weight < 6 = 0
  | otherwise = weight `quot` 3 - 2

tyrannyOfTheRocketEquation :: Int -> Int
tyrannyOfTheRocketEquation weight = 
  let baseFuel = calcFuel weight
  in if baseFuel == 0
    then 0
    else baseFuel + tyrannyOfTheRocketEquation baseFuel

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let weights = parseWeights contents
  let fuels = map tyrannyOfTheRocketEquation weights
  print (sum fuels)
