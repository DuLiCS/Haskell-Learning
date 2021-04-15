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


factorial :: (Integral a) => a -> a

factorial 0 = 1
factorial x = x * factorial (x - 1)

head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x


max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b


myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b   = GT
  | a == b  = EQ
  | a < b   = LT


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ ". "
   where (f:_) = firstname
         (l:_) = lastname

calBmi :: (RealFloat a) => [(a, a)] -> [a]
calBmi xs = [bmi w h | (w, h) <- xs]
   where bmi weight height = weight / height ^ 2







































