integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = x
 where
  n = 1000
  h = (b - a) / n
  
  x = h * ( (f a + f b) / 2 + sum' 0 f (a + h) (b -h) )
  sum' acc f x y
   | x < y - (h/2) = sum' (acc + f x) f (x + h) y
   | otherwise = acc 
