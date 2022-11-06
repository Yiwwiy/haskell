
--task1 двойной факториал
dFact :: Integer -> Integer
dFact n  
   | n < 2  = 1
   | otherwise = n * dFact(n-2)
 

--task2 сумма цифр в числе
sumOfDigit :: Integer -> Int
sumOfDigit n 
   | n == 0  = 0
   | otherwise = fromIntegral (n `rem` 10) + sumOfDigit (n `quot` 10)
   

--task3  возвращает степень 2
powOf2 :: Integer -> Int
powOf2 n = degree n 0  

degree :: Integer -> Int -> Int
degree x y  
    | x == 1   = y
    | x < 1 || odd x = -1
    | otherwise = degree (x `div` 2) (y + 1)
 
--task4 количество точек, в которых окружности пересекаются
qntPoints :: Int -> Int -> Int -> Int -> Int -> Int -> Int
qntPoints x1 y1 r1 x2 y2 r2 
   | d > r  || r' > d = 0
   | d == r || r' == d = 1 
   | otherwise  = 2
   where d = (x1 - x2)^2 + (y1 - y2)^2
         r = (r1 + r2)^2
         r' = (r1 - r2)^2 

--task 5  функция sin
fmod :: Double -> Double -> Double
fmod x y =
    if abs x < y then
        x
    else
        x - y * fromIntegral (truncate z) where z = x / y
        

mySin :: Double -> Double -> Double
mySin x eps = 
     iter l eps 3 l l 
    where l = fmod x pi
    
iter :: Double -> Double -> Double -> Double -> Double -> Double
iter x eps n temp result =
    if abs temp > eps then
        iter x eps (n + 2) 
            (-temp * x * x / (n*n - n))
            (result + (-temp * x * x / (n*n - n)))
    else
        result 

--task 6 безаргументные функции

-- x^y
func1 :: Integer -> Integer -> Integer
func1 = (^)

-- (x+1)^y
func2 = (^) . (+1)

-- x^(y-3)
func3 :: Double -> Double -> Double
func3  = flip (flip (**) . flip (-)3)

--(x+1)^(y-3)
func4 :: Integer -> Integer -> Integer
func4 = flip (func2 . flip (-) 3)

