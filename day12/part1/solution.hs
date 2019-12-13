import System.Environment

data Moon = Moon {
  name :: [Char]
  , x :: Int
  , y :: Int
  , z :: Int
  , dx :: Int
  , dy :: Int
  , dz :: Int
} deriving (Show, Eq)

moon :: Moon
moon = Moon "DEADBEEF" 0 0 0 0 0 0

toArg :: [Char] -> [Char]
toArg = tail . dropWhile (/='=')

takeInt :: [Char] -> (Int, [Char])
takeInt str = 
  let (int, tail) = span (`notElem` [',', '>']) str
    in (read int :: Int, tail)

positions :: ([Char], [Char]) -> Moon
positions (name, line) = 
  let first = toArg line
      (x, tail) = takeInt first
      second = toArg tail
      (y, tail2) = takeInt second
      third = toArg tail2
      (z, _) = takeInt third
    in moon {name = name, x = x, y = y, z = z }

cmp :: Int -> Int -> Int
cmp l r
  | l < r = 1
  | l > r = -1
  | otherwise = 0

calcGravity :: Moon -> Moon -> (Int, Int, Int)
calcGravity Moon { x=x1, y=y1, z=z1 } Moon { x=x2, y=y2, z=z2} = (cmp x1 x2, cmp y1 y2, cmp z1 z2)

add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

gravitize :: Moon -> [Moon] -> (Int, Int, Int)
gravitize target candidates = 
  let effects = [calcGravity target c | c <- candidates, target /= c]
    in foldl1 add effects

applyGravity :: Moon -> (Int, Int, Int) -> Moon
applyGravity moon (d2x, d2y, d2z) =
  let Moon {dx=dx, dy=dy, dz=dz} = moon
    in moon {dx = dx + d2x, dy = dy + d2y, dz = dz + d2z}

move :: Moon -> Moon
move moon = 
 let Moon {x=x, y=y, z=z, dx=dx, dy=dy, dz=dz} = moon
   in moon {x = x + dx, y = y + dy, z = z + dz}

tick :: [Moon] -> [Moon]
tick moons = 
  let gravitations = [ gravitize m moons | m <- moons ]
    in map (move.uncurry applyGravity) $ zip moons gravitations

ntick :: Int -> [Moon] -> [Moon]
ntick 0 moons = moons
ntick x moons = ntick (x-1) $ tick moons

energy :: Moon -> Int
energy Moon {x=x, y=y, z=z, dx=dx, dy=dy, dz=dz} = (abs x + abs y + abs z) * (abs dx + abs dy + abs dz)

moonNames :: [[Char]]
moonNames = ["Io", "Europa", "Ganymede", "Callisto"]

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let moons = map positions $ zip moonNames $ lines contents
  print . sum $ map energy $ ntick 1000 moons
