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

count :: Eq a => a -> [a] -> Int
count target = length.filter (==target)

zeroSort :: [Int] -> [Int] -> Ordering
zeroSort left right = compare (count 0 left) (count 0 right)

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let pixels = [ read [c] :: Int | c <- filter (/='\n') contents ]
  let layers =  extractLayers (imageWidth * imageHeight) pixels
  let targetLayer = head <$> sortBy zeroSort <$> layers
  let ones = count 1 <$> targetLayer
  let twos = count 2 <$> targetLayer
  let result = (*) <$> ones <*> twos
  case result of
    Just r -> print r
    Nothing -> print "Unable to calculate"
