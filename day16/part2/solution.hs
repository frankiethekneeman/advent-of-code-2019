import System.Environment
import Data.Char

parseSignal :: [Char] -> [Int]
parseSignal str = [ read [i] :: Int | i <- str, i <= '9', i >= '0' ]

asDigits :: [Int] -> Int
asDigits = read . map (chr .(+ fromEnum '0'))

getOffset :: [Int] -> Int
getOffset = asDigits . take 7

fftStepDeep :: [Int] -> [Int]
fftStepDeep = map ((`mod` 10) . abs) . scanr1 (+)

fftDeep :: Int -> [Int] -> [Int]
fftDeep 0 input = input
fftDeep k input = fftDeep (k-1) $ fftStepDeep input

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let signal = parseSignal contents
  let messageOffset = getOffset signal
  let realSignal = drop messageOffset . concat $ replicate 10000 signal
  print $ asDigits $ take 8 $ fftDeep 100 realSignal
