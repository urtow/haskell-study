sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x  = s' 0 0 (abs x)
 where 
  s' 0 0 0   = (0,1)
  s' sum n 0 = (sum,n)
  s' sum n x = s' (sum + rem x 10) (n + 1) (quot x 10)
