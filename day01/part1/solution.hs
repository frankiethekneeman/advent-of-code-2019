import System.Environment

parseWeights :: String -> [Int]
parseWeights str = 
    [ read i :: Int | i <- lines str ]

calcFuel :: Int -> Int
calcFuel weight = weight `quot` 3 - 2

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let weights = parseWeights contents
  let fuels = map calcFuel weights
  print (sum fuels)
