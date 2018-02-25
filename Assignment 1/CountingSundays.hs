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


sundays1' :: Integer -> Integer -> Integer
sundays1' start end = sundays' 0 start 1
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' acc y m
      | y > end              = acc
      | dayOfWeek y m 1 == 1 = sundays' (acc+1) nextY nextM
      | otherwise            = sundays' acc nextY nextM
      where
        nextY = if m < 12 then y else y + 1
        nextM = if m < 12 then m + 1 else 1


leap :: Integer -> Bool
leap y = (mod y 4 == 0) && (mod y 100 /= 0) || (mod y 400 == 0)


daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y
  | m == 2            = if leap y then 29 else 28
  | m == 4 || m == 6  = 30
  | m == 9 || m == 11 = 30
  | otherwise         = 31


sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays2' 0 2 start 1
  where
    sundays2' :: Integer -> Integer -> Integer -> Integer -> Integer
    sundays2' acc acc2 y m
      | y > end            = acc
      | mod weekday 7 == 0 = sundays2' (acc + 1) weekday nextY nextM
      | otherwise          = sundays2' acc weekday nextY nextM
        where
          nextY   = if m < 12 then y else y + 1
          nextM   = if m < 12 then m + 1 else 1
          days    = daysInMonth m y
          weekday = acc2 + mod days 7
