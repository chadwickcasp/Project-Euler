import Data.List

-- main = print(sum_five_three_multiples 1000)
main = print (foldr (+) 0 (filter even (fib_list 0 4000000)))
-- main = print()
-- main = print(1000 `mod` 3)
-- multiples_up_to 1 1 = [1]
-- multiples_up_to 1 n = [1, n]

-- multiples_up_to m n = 
--     if n == 1 
--         then [1]
--         else if n == m then [1, m]
--             else if n `mod` m /= 0
--                 then multiples_up_to m n-1
--                 else multiples_up_to m (n/m) ++ [n]

fib :: (Integral a) => a -> a
fib n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

-- fib_less_than

fib_up_to :: (Integral a) => a -> a -> [a]
fib_up_to n m
    | n == 1 = [1]
    | n == 2 = [1, 2]
    | last (fib_up_to n m) >= m = fib_up_to (n-1) m
    | otherwise = fib_up_to (n-2) m ++ [last (fib_up_to (n-2) m) + last (fib_up_to (n-1) m)]

fib_list :: (Integral a) => a -> a -> [a]
fib_list n m
    | fib (n+1) >= m = []
    | otherwise = fib_list (n+1) m ++ [fib (n+1)]



fib_sum :: (Integral a) => a -> a
fib_sum n
    | n == 1 = 1
    | n == 2 = 3
    | otherwise = fib_sum (n-1) + fib_sum (n-2)

-- three_multiples 333 = 999



-- five_three_multiples :: Eq a => [a] -> [a]
-- i = 3 `mod` 3
