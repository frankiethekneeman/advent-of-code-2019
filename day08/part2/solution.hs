import System.Environment
import Data.List

imageWidth = 25
imageHeight = 6

chunk :: Int -> [a] -> [[a]]
chunk n list
  | length list <= n = [list]
  | otherwise =
    let (chunked, rest) = splitAt n list
      in chunked:(chunk n rest)

extractLayers :: Int -> [Int] -> Maybe [[Int]]
extractLayers n input
  | l `mod` n == 0 = Just $ chunk n input
  | otherwise = Nothing
  where l = length input

layersToPixels :: [[Int]] -> [[Int]]
layersToPixels [] = []
layersToPixels layers
  | (length $ head layers) == 0 = []
  | otherwise =
    let pixel = map head layers
        remaining = map tail layers
      in pixel:(layersToPixels remaining)

flatten :: [[Int]] -> [Int]
flatten pixels = map (head.(dropWhile (==2))) pixels

render :: Int -> [Int] -> Maybe [Char]
render width pixels
  | (length pixels) `mod` width /= 0 = Nothing
  | otherwise =
    let rows = chunk width pixels
        digits = foldl1 (++) $ map ((++"\n").(foldl1 (++)).(map show)) rows
      in Just [ case c of
                  '0' -> ' '
                  '1' -> '•'
                  otherwise -> c
                |  c <- digits ]

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let pixels = [ read [c] :: Int | c <- filter (/='\n') contents ]
  let layers =  extractLayers (imageWidth * imageHeight) pixels
  let pixels = layersToPixels <$> layers
  let flat = flatten <$> pixels
  case (render imageWidth =<< flat) of
    Just img -> putStrLn img
    Nothing -> print "Failed to calculate."
