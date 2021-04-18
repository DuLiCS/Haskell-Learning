import Data.List


input = [[3, 4, 0, 0],
         [2, 0, 3, 0],
         [0, 3, 0, 2],
         [0, 0, 1, 3]]

testEmpty = [[3, 4, 0, 0],
         [],
         [0, 3, 0, 2],
         [0, 0, 1, 3]]

blankListCounter [] = 0
blankListCounter (x:xs) = (if x == [] then 1 + blankListCounter xs else blankListCounter xs)

inputTranspose = transpose input

elementChecker st = [ x | x <- st, not(x `elem` [1..4])]


--preProcess
zeroCounter [] = 0
zeroCounter (x:xs) = (if x == 0 then 1 + zeroCounter xs else zeroCounter xs)


zerosList [] = [5]
zerosList [[],_,_,_] = error "empty input"
zerosList [_,[],_,_] = error "empty input"
zerosList [_,_,[],_] = error "empty input"
zerosList [_,_,_,[]] = error "empty input"
zerosList (x:xs) = zeroCounter x : zerosList xs

blockToList

listToBlock