import Data.Foldable (maximumBy, for_)
import Data.Ord (comparing)

test :: Integral n => n -> n -> n -> n
test b k n = k * lastDigit + otherDigits
  where
    lastDigit =  n `mod` b
    otherDigits = n `div` b

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

iteration :: Integral a => a -> a -> a -> [a]
iteration b k = iterate (test b k)

findHeadAndCycle :: Integral a => a -> a -> a -> ([a], [a])
findHeadAndCycle b k n = findCycle $ iteration b k n

periodicity :: Integral a => a -> a -> a -> (Int, Int)
periodicity b k n = (length xs, length ys)
  where
    (xs, ys) = findHeadAndCycle b k n


periodicitiesTagged :: Integral b => b -> [b] -> [(b, b, Int, Int)]
periodicitiesTagged b testRange = do
  k <- testRange
  n <- [1]
  let (x, y) = periodicity b k n
  pure (k, n, x, y)

longestCycle :: Integral b => b -> [b] -> (b, b, Int, Int)
longestCycle b testRange = maximumBy (comparing (\(_, _, _, y) -> y)) (periodicitiesTagged b testRange)

-- Not sure why, but `periodicities b` sequence matches relevant multiplicative orders.
-- E.g.,
-- b, url
--  2, https://oeis.org/A002326
--  3, https://oeis.org/A003572
--  4, https://oeis.org/A003574
--  5, https://oeis.org/A217852
-- 10, https://oeis.org/A128858
periodicities :: Integral b => b -> [b] -> [Int]
periodicities b testRange = (\(_, _, _, y) -> y) <$> periodicitiesTagged b testRange

insertions :: Integral b => b -> [b] -> [Int]
insertions b testRange = (\(_, _, x, _) -> x) <$> periodicitiesTagged b testRange

multiplicativeOrderCheck :: (Integral a, Integral b) => a -> b -> a -> Bool
multiplicativeOrderCheck b m n = ( b ^ m - 1 ) `mod` (b * n - 1) == 0

main :: IO ()
main = do
  let 
    ns = [1..20]
    bs = [2..20]
    checksForallBases = do
      b <- bs
      let 
        ms = periodicities b ns
        checksForallSubscripts = zipWith (multiplicativeOrderCheck b) ms ns
      pure $ and checksForallSubscripts
  print $ and checksForallBases