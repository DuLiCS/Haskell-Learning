import Data.Char
import Data.Array
import Data.List
import System.Environment

{-
the input is like this:
	input = [[3,4,0,0],
            [2,0,3,0],
            [0,3,0,2],
            [0,0,1,3]]
the
-}

type Indice4D = (Int,Int,Int,Int)
type Indice2D = (Int,Int)
type Digit = Int
type Grid =  Array Indice4D Digit

testString = " 34 __ " ++
                  " 2_ 3_ " ++
                  " _3 _2 " ++
                  " __ 13 "

puzzle = [[3,4,0,0],
         [2,0,3,0],
         [0,3,0,2],
         [0,0,1,3]]



ij :: Indice4D -> Indice2D
ij (x, y, x', y') = (i, j)
    where i = 2 * x + x'
          j = 2 * y + y'

xy :: Indice2D -> Indice4D
xy (i,j) = (x, y, x', y')
    where (x, x') = divMod i 2
          (y, y') = divMod j 2


i4D = ((0,0,0,0),(1,1,1,1))   
r4D = range i4D               
i2D = ((0,0),(3,3))           
r2D = range i2D

parseGrid x = array i4D $ zip (map xy r2D) (concat x)


solve :: Grid -> [Grid]
solve g = fillGrid' g ps
    where
        ps = filter (\p -> g!p == 0) r4D


fillGrid :: Grid -> [Indice4D] -> [Grid]
fillGrid g [] = [g]
fillGrid g (p:ps) = concat [fillGrid (g // [(p, d)]) ps | d <- candidates g p]


fillGrid' :: Grid -> [Indice4D] -> [Grid]
fillGrid' g [] = [g]
fillGrid' g ps = concat [fillGrid' (g // [(p, d)]) ps' | d <- qs]
    where
        (_, p, qs) = minimum [(length qs, p, qs) | p <- ps, let qs = candidates g p]
        ps' = delete p ps

candidates :: Grid -> Indice4D -> [Digit]
candidates g (x,y,x',y') = [1..9] \\ (row++col++sqr)
    where row = [n | x  <- r3, x' <- r3, let n = g!(x,y,x',y'), n /= 0]
          col = [n | y  <- r3, y' <- r3, let n = g!(x,y,x',y'), n /= 0]
          sqr = [n | x' <- r3, y' <- r3, let n = g!(x,y,x',y'), n /= 0]
          r3 = [0..2]

















