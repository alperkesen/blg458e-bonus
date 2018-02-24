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

