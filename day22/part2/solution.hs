import System.Environment
import Data.List
import Data.Array
import qualified Data.Set as Set
import Debug.Trace

-- Stole this shit from rorsetta code: https://rosettacode.org/wiki/Modular_inverse#Haskell
-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Integer -> Integer -> Maybe Integer
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x
 
-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)
-- End of stolen code.  I promise to put this in my 5 minute talk

data Instruction = NewStack | Cut Integer | Increment Integer deriving (Show)

-- True arithmetic modulo operator.  0 <= a % b < b, always.
(%) :: Integer -> Integer -> Integer
(%) a b
  | a < 0 = ((a `mod` b) + b) `mod` b
  | otherwise = a `mod` b


parseInstruction :: [Char] -> Instruction
parseInstruction "deal into new stack" = NewStack
parseInstruction line
  | "cut " `isPrefixOf` line = Cut $ read $ drop 4 line
  | otherwise = Increment $ read $ drop 20 line

data ModularLinearEquation = ModularLinearEquation {
  slope :: Integer
  , intercept :: Integer
  , modulo :: Integer
} deriving (Eq)

instance Show ModularLinearEquation where
  show (ModularLinearEquation a b m) = (show a) ++ "x + " ++ (show b) ++ " % " ++ (show m)

unapplyAsMLE :: Integer -> Instruction -> ModularLinearEquation
unapplyAsMLE l NewStack = ModularLinearEquation (-1) (l - 1) l
unapplyAsMLE l (Cut n) = ModularLinearEquation 1 n l
unapplyAsMLE l (Increment i) = case modInv i l of
  Just inv -> ModularLinearEquation inv 0 l

normalize :: ModularLinearEquation -> ModularLinearEquation
normalize (ModularLinearEquation a b m) = ModularLinearEquation (a % m) (b % m) m

apply :: Integer -> ModularLinearEquation -> Integer
apply x (ModularLinearEquation a b m) = ((a * x) + b) % m

compose :: ModularLinearEquation -> ModularLinearEquation -> ModularLinearEquation
compose (ModularLinearEquation a b m1) (ModularLinearEquation c d m2)
  | m1 == m2 = ModularLinearEquation a' b' m1
  where a' = (a * c) % m1
        b' = ((a * d) + b) % m1

repeatCompose :: Int -> ModularLinearEquation -> ModularLinearEquation
repeatCompose 0 f = ModularLinearEquation 1 0 (modulo f)
repeatCompose 1 f = f
repeatCompose n f
  | r == 0 = doubled
  | r == 1 = compose doubled f
  where (q, r) = n `quotRem` 2
        toDouble = repeatCompose q f
        doubled = compose toDouble toDouble

deckSize :: Integer
deckSize = 119315717514047

indexOfInterest :: Integer
indexOfInterest = 2020

repetitions :: Int
repetitions = 101741582076661

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let instructions = map parseInstruction $ lines contents
  let equations = map (normalize . unapplyAsMLE deckSize) $ instructions
  let composed = foldl1 compose $ equations
  let repeated = repeatCompose repetitions composed
  print $ apply indexOfInterest repeated
