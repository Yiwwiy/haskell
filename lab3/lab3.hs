import Data.List
import Data.Tuple
import Data.Foldable
import Data.Function

--task1
num2lst n 
    | n <= 9 = [n]
    | otherwise = num2lst (n `div` 10) ++ [fromIntegral n `mod` 10]
      

lst2num ::  [Int] -> Integer

lst2num lst =  if null lst then 0 else toInteger (last lst) + 10 * lst2num (init lst)
lst2num' lst = foldl (\x y -> x*10 + y) 0  lst
 
--task2
--создаёт список квадратов от 0 до n
toSquares n = [n*n | n <- [0 .. n]]

--a) рекурсия
sumSquares1 :: Num a => [a] -> a
sumSquares1 lst = if null lst  then 0 else head lst ^ 2 + sumSquares1 (tail lst)  

-- генерация списков
sumSquares lst = sum [h^2 | h <- lst]
 
--б) хвостовая рекурсия 
sumSquares2 :: Num a => [a] -> a
sumSquares2 lst = sumSqIt lst 0

sumSqIt :: Num t => [t] -> t -> t
sumSqIt lst res = if null lst then res
                  else sumSqIt t (res + h^2) where h = head lst
                                                   t = tail lst
     
--в) функции высших порядков
sumSquares3 :: Num a => [a] -> a
sumSquares3 = foldr (\x -> (+) (x ^ 2)) 0

-- task3
posMax :: [String] -> String
posMax strs =  [maximum y | y <- transpose strs]

-- task4 минимальный полярный угол
distance (x, y) = sqrt (x*x + y*y)

--компакатор
comp x y 
  | angle1 > angle2 || distance p1 < distance p2 = GT
  | angle1 < angle2 || distance p1 > distance p2 = LT 
  | otherwise = EQ
  where angle1 = fst x
        angle2 = fst y
        p1 = snd x
        p2 = snd y

atan2wrapper x y
    | angle < 0 = angle + 2 * pi
    | otherwise = angle
    where angle = atan2 x y

minAngle :: [(Double,Double)] -> (Double,Double)
minAngle points = snd (minimumBy comp [(atan2wrapper (snd p) (fst p), p) | p <- points])

--task5 медиана набора чисел
upperHalf :: (Fractional a, Ord a) => [a] -> [a]
upperHalf lst
  | even (length lst) = filter (> x) lst  
  | otherwise = filter (> b) lst
  where
    x = a + (b - a) / 2 
    a = sort lst !! (i - 1) 
    b = sort lst !! i 
    i = length lst `div` 2
    

--task6 воввращает пару (наибольший элемент по вхождению , строка без этого элемента)
--количество вхождений элемента список
count w = length . filter ( == w)

mostFrequent :: String -> (Char,String)
mostFrequent str = (x, filter (x /=) str) where x = snd (maximumBy (compare `on` fst) [(count c str, c) | c <- str])

