-- 
--   BLG 458E - Functional Programming
--   Bonus Assignment 1
--
--   Name: Ekrem Alper Kesen
--   ID:   150150018
--
--

module CountingSundays where


dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = mod (d + t + k + div k 4 + div j 4 - 2 * j) 7
  where
    y'
      | m <= 2    = y - 1
      | otherwise = y
    m'
      | m <= 2    = m + 12
      | otherwise = m

    k = mod y' 100
    j = div y' 100
    t = div (13 * (m' + 1)) 5


sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' y m
      | y > end   = 0
      | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
      where
        nextY = if m < 12 then y else y + 1
        nextM = if m < 12 then m + 1 else 1
        rest = sundays' nextY nextM

