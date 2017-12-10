import Data.Maybe

checkDate :: Int -> Int -> Int -> Bool
checkDate d m y = d>0 && m>0 && m<13 && y>=0 && d <=
    (if m `elem` [4,6,9,11] then 30
     else 
        if m == 2 then
            if (y `mod` 400 == 0) || (y `mod` 100 /= 0) && 
                (y `mod` 4 == 0) then 29
            else 28
        else 31)
 
nod :: Integer -> Integer -> Integer
nod a b | a == b = a
        | a > b = if a `mod` b == 0 then b else nod b (a - b)
        | otherwise = nod b a

t_pow :: Integer -> Integer -> Integer
t_pow m n | n < 0 = error "Negative component"
          | n == 0 = 1
          | n == 1 = m
          | n `mod` 2 == 1 = m * t_pow m (n-1)
          | n `mod` 2 == 0 = t_pow (m*m) (div n 2)
