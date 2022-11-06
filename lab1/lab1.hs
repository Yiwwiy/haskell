--task 2
gcd' :: Int -> Int -> Int
gcd' x 0 = x 
gcd' x y = gcd' y (x `rem` y)



--task 3
--1 вариант
divisible :: Int->[Int]->Bool
divisible x y
    | y == []               = False
    | x `mod` (head y) == 0 = True
    | otherwise             = divisible x (tail y)

          
         
prime :: Int-> Bool
prime n
        | n < 2     = False
        | n == 2    = True
        | even n    = False
        | otherwise =
          not (divisible n [3,5..n-2])   
          
--2 вариант          
prime' n = n == 2 || (n > 2  &&  odd n  &&  [k | k <- [3,5 .. n-2], n `mod` k == 0] == [])

 --task 4
reverseNumber:: Integer->Integer
reverseNumber n = rev 0 n
    where 
        rev x 0 = x
        rev x n = rev (x * 10 + mod n 10) (div n 10)

-- task 4
functionRoot :: Int -> Int -> Int -> Double  
functionRoot a b c = if d < 0 then 0/0 else (max x1 x2)
                        where
                          x1 = e + sqrt d / (2 * fromIntegral a)
                          x2 = e - sqrt d / (2 * fromIntegral a)
                          d = fromIntegral (b * b - 4 * a * c)
                          e = - fromIntegral b / (2 * fromIntegral a)

--task 5
maxRoot :: Int -> Int -> Int -> Double
maxRoot a b c = 
  let 
    a1 = fromIntegral(a) :: Double
    b1 = fromIntegral(b) :: Double
    c1 = fromIntegral(c) :: Double
    d = b1 ^ 2 - 4 * a1 * c1
    x1 = (-b1 + sqrt(d)) / (2 * a1)
    x2 = (-b1 - sqrt(d)) / (2 * a1)
  in max x1 x2                          
   




