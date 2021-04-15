doubleMe x = x + x

doubleUs x y = x + x + y + y

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

sayMe :: (Integral a) => a -> String   
sayMe 1 = "One!"   
sayMe 2 = "Two!"   
sayMe 3 = "Three!"   
sayMe 4 = "Four!"   
sayMe 5 = "Five!"   
sayMe x = "Not between 1 and 5"

first :: (a, b, c) -> a   
first (x, _, _) = x   

maxInList [] = []
maxInList [x] = x
maxInList (x:xs) = max x (maxInList xs)


reverseL [] = []
reverseL [x] = [x]
reverseL (x:xs) = reverseL xs ++ [x]


quicksort [] = []
quicksort (x:xs) = (quicksort [y | y <- xs, y<x]) ++ [x] ++ (quicksort [y | y <-xs, y>=x])