import Data.List

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Int

boxSize :: Int
boxSize = 2

values :: [Value]
values = [1 .. 4]

empty :: Value -> Bool
empty = (== 0)

single :: [a] -> Bool
single [_] = True
single _   = False

puzzle :: Grid
puzzle = [[0, 4, 0, 0],
         [2, 0, 0, 4],
         [0, 0, 0, 0],
         [4, 0, 0, 3]]

valid :: Grid -> Bool
valid g = all noDups (rows g) &&
          all noDups (cols g) &&
          all noDups (boxes g)

noDups :: (Eq a, Num a) => [a] -> Bool
noDups [] = True
noDups (x : xt) = if x == 0 then  (noDups xt) else (not (elem x xt) && noDups xt) 

rows :: Matrix a -> [Row a]
rows =  id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
        where
          pack   = split . map split
          split  = chop boxSize
          unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

collapse =  sequence . map sequence

choices g = map (map choice) g
            where
              choice v = if empty v then values else [v]

solveBrute :: Grid -> [Grid]
solveBrute =  filter valid . collapse . choices


prune =  pruneBy boxes . pruneBy cols . pruneBy rows
         where pruneBy f = f . map reduce . f

reduce xss =  [xs `minus` singles | xs <- xss]
              where singles = concat (filter single xss)

xs `minus` ys = if single xs then xs else xs \\ ys

solvePrune :: Grid -> [Grid]
solvePrune =  filter valid . collapse . prune . choices


solveFixPrune :: Grid -> [Grid]
solveFixPrune = filter valid . collapse . fix prune . choices

fix :: Eq a => (a -> a) -> a -> a
fix f x =  if x == x' then x else fix f x'
           where x' = f x