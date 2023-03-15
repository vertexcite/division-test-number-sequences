import Data.Foldable (maximumBy, for_)
import Data.Ord (comparing)

test :: Integral a => a -> a -> a
test k n = k * unitsDigit + shiftedRight where
  unitsDigit = n `mod` 10
  shiftedRight = n `div` 10

-- Source of below function: https://wiki.haskell.org/Floyd's_cycle-finding_algorithm
findCycle :: Eq a => [a] -> ([a],[a])
findCycle xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x == y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys

iteration :: Integral a => a -> a -> [a]
iteration k = iterate (test k)

findHeadAndCycle :: Integer -> Integer -> ([Integer], [Integer])
findHeadAndCycle = (findCycle .) . iteration

periodicity :: Integer -> Integer -> (Int, Int)
periodicity k n = (length xs, length ys)
  where
    (xs, ys) = findHeadAndCycle k n


periodicitiesTagged :: [(Integer, Integer, Int, Int)]
periodicitiesTagged = do
  k <- [1..1000]
  n <- [1]
  let (x, y) = periodicity k n
  pure (k, n, x, y)

longestCycle :: (Integer, Integer, Int, Int)
longestCycle = maximumBy (comparing (\(_, _, _, y) -> y)) periodicitiesTagged

-- This sequence matches https://oeis.org/A128858, not sure why
periodicities :: [Int]
periodicities = (\(_, _, _, y) -> y) <$> periodicitiesTagged

insertions :: [Int]
insertions = (\(_, _, x, _) -> x) <$> periodicitiesTagged

displayLotsOfOutput :: IO ()
displayLotsOfOutput = do
  for_ [1..20] $ \k -> do
    print k
    for_ [0..2*10*k] $ \n -> do
      let (x, y) = periodicity k n
      print (k, n, x, y)
    putStrLn ""
  
main :: IO ()
main = print periodicities