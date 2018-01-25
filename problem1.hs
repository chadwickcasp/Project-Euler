import Data.List

main = print (sum_five_three_multiples_below_thousand)

multiples_up_to :: (Integral a) => a -> a -> [a]
multiples_up_to m n
    | n == m = [m]
    | n `mod` m /= 0 = multiples_up_to m (n-1)
    | otherwise = multiples_up_to m (n-m) ++ [n]


multiples_of_three_five = multiples_up_to 5 1000 ++ multiples_up_to 3 1000
u_multiples_of_three_five = nub multiples_of_three_five
sum_five_three_multiples_below_thousand = 
    (foldr (+) 0 u_multiples_of_three_five) - 1000

