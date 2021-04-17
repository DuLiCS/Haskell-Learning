
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

 



--compareWithHundred :: (Num a, Ord a) => a -> Ordering
--compareWithHundred  = compare 100


--zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith' _[]_ = []
--zipWith' _ _ [] = []
--zipWith' f (x: xs) (y: ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f  = g
  where g x y = f y x 


myLast :: [a] -> a
myLast [] = error "empty"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = foldr1 (const id)


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



myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

removeNoneUppercase :: [Char] -> [Char]
removeNoneUppercase  st = [ x | x <- st, x `elem` ['A'..'Z']]


lucky :: (Integral a) => a -> String
lucky 7 = "It's seven"
lucky x = "out of luck"


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


second :: (a, b, c) -> b
second (_, y, _) = y

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
      let sideArea = 2 * pi * r * h
          topArea = pi * r ^ 2
      in sideArea + 2 * topArea 


calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]


maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "empty"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x


zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =  
    let smallerSorted = quicksort [a | a <- xs, a <= x] 
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in smallerSorted ++ [x] ++ biggerSorted

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100  

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs

fliter :: (a -> Bool) -> [a] -> [a]
fliter _ [] = []
fliter p (x:xs)
    | p x    = x : fliter p xs
    | otherwise = fliter p xs







