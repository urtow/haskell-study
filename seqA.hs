seqA :: Integer -> Integer
seqA n
  | n == 0    = 1
  | n == 1    = 2
  | n == 2    = 3
  | otherwise = let
      helper f s t 0 = f
      helper f s t n = helper s t ((t + s) - 2 * f) (n - 1)
    in helper 1 2 3 n
