import System.Environment
import Data.Char

parseSignal :: [Char] -> [Int]
parseSignal str = [ read [i] :: Int | i <- str, i <= '9', i >= '0' ]

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

multipliers :: Int -> [Int]
multipliers n = tail.cycle.concat $ map (replicate n) basePattern

multiplierList :: [[Int]]
multiplierList = map multipliers [1..]

fftUnit :: [Int] -> [Int] -> Int
fftUnit signal multipliers = (abs . sum $ zipWith (*) multipliers signal) `mod` 10

fftStep :: [Int] -> [Int]
fftStep input = take (length input) $ map (fftUnit input) multiplierList

fft :: Int -> [Int] -> [Int]
fft 0 input = input
fft k input = fft (k-1) . fftStep $ input

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let signal = parseSignal contents
  putStrLn $ map (chr.(+(fromEnum '0'))) $ take 8 $ fft 100 signal
