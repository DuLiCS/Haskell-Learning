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


-- quicksort [] = []
-- quicksort (x:xs) = (quicksort [y | y <- xs, y<x]) ++ [x] ++ (quicksort [y | y <-xs, y>=x])


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


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

 


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred  = compare 100

applyTwice :: (a -> a ) -> a -> a
applyTwice f x = f(f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _[]_ = []
zipWith' _ _ [] = []
zipWith' f (x: xs) (y: ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f  = g
  where g x y = f y x 


myLast :: [a] -> a
myLast [] = error "empty"
myLast [x] = x
myLast (_:xs) = myLast xs

--solution1
myButLast :: [a] -> a
myButLast x = reverse x !! 1

--solution2
myButLast' [x,_] = x
myButLast' (_:xs) = myButLast' xs



--solution1
elementAt :: [a] -> Int -> a
elementAt list n = list !! (n - 1)

--solution2
elementAt' (x:_) 1 = x
elementAt' [] _ = error "out of bounds"
elementAt' (_:xs) k = elementAt' xs (k - 1)


















