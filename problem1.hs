import Data.List

-- main = print(sum_five_three_multiples 1000)
main = print (sum_five_three_multiples_below_thousand)
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

multiples_up_to :: (Integral a) => a -> a -> [a]
multiples_up_to m n
    | n == m = [m]
    | n `mod` m /= 0 = multiples_up_to m (n-1)
    | otherwise = multiples_up_to m (n-m) ++ [n]


multiples_of_three_five = multiples_up_to 5 1000 ++ multiples_up_to 3 1000
u_multiples_of_three_five = nub multiples_of_three_five
sum_five_three_multiples_below_thousand = 
    (foldr (+) 0 u_multiples_of_three_five) - 1000

-- three_multiples 333 = 999



-- five_three_multiples :: Eq a => [a] -> [a]
-- i = 3 `mod` 3
