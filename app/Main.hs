import Data.Foldable (maximumBy, for_)
import Data.Ord (comparing)
import Data.Digits (digits, unDigits)

-- Source of below function: https://stackoverflow.com/questions/29153110/left-pad-a-haskell-list
lpad :: Num a => Int -> [a] -> [a]
lpad m xs = replicate (m - length ys) 0 ++ ys
    where ys = take m xs


test :: Integral n => n -> n -> n -> n
test b k n = unDigits b added
  where
    ds = digits b n
    lastDigit = last ds
    otherDigits = init ds
    lk = lastDigit * k
    lkds = digits b lk
    added = lkds `addDigits` otherDigits

addDigits :: Integral a => [a] -> [a] -> [a]
addDigits xs ys = zipWith (+) (lpad m xs) (lpad m ys)
  where
    m = max (length xs) (length ys) 

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


periodicitiesTagged :: Integral b => b -> [(b, b, Int, Int)]
periodicitiesTagged b = do
  k <- [1..20]
  n <- [1]
  let (x, y) = periodicity b k n
  pure (k, n, x, y)

longestCycle :: Integral b => b -> (b, b, Int, Int)
longestCycle b = maximumBy (comparing (\(_, _, _, y) -> y)) (periodicitiesTagged b)

-- `periodicities 10` sequence matches https://oeis.org/A128858, not sure why
periodicities :: Integral b => b -> [Int]
periodicities b = (\(_, _, _, y) -> y) <$> periodicitiesTagged b

insertions :: Integral b => b -> [Int]
insertions b = (\(_, _, x, _) -> x) <$> periodicitiesTagged b

main :: IO ()
main = do
  for_ [2..10] $ \b -> do
    print b
    print $ periodicities b
    putStrLn ""