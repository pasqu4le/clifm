module ListZipper where

-- implementation of a list zipper that always has at least one item

data Zipper a = Zipper {current :: a, preceding :: [a], following :: [a]} deriving Show

start :: Zipper a -> Zipper a
start (Zipper c ps fs) = Zipper x [] xs
  where (x:xs) = reverse ps ++ (c:fs)

end :: Zipper a -> Zipper a
end (Zipper c ps fs) = Zipper x xs []
  where (x:xs) = reverse fs ++ (c:ps)

replace :: a -> Zipper a -> Zipper a
replace item (Zipper _ prevs foll) = Zipper item prevs foll

-- NOTE: needs a default value in case the zipper only has one item
remove :: a -> Zipper a -> Zipper a
remove _ (Zipper c ps (f:fs)) = Zipper f ps fs
remove _ (Zipper c (p:ps) []) = Zipper p ps []
remove item zipper = replace item zipper

insertLeft :: a -> Zipper a -> Zipper a
insertLeft item (Zipper c ps fs) = Zipper c (item:ps) fs

insertRight :: a -> Zipper a -> Zipper a
insertRight item (Zipper c ps fs) = Zipper c ps (item:fs)

insertLeftAndFocus :: a -> Zipper a -> Zipper a
insertLeftAndFocus item (Zipper c ps fs) = Zipper item ps (c:fs)

insertRightAndFocus :: a -> Zipper a -> Zipper a
insertRightAndFocus item (Zipper c ps fs) = Zipper item (c:ps) fs

next :: Zipper a -> Zipper a
next (Zipper c ps []) = Zipper c ps []
next (Zipper c ps (f:fs)) = Zipper f (c:ps) fs

prev :: Zipper a -> Zipper a
prev (Zipper c [] fs) = Zipper c [] fs
prev (Zipper c (p:ps) fs) = Zipper p ps (c:fs)

nextCiclical :: Zipper a -> Zipper a
nextCiclical (Zipper c ps []) = start (Zipper c ps [])
nextCiclical (Zipper c ps (f:fs)) = Zipper f (c:ps) fs

prevCiclical :: Zipper a -> Zipper a
prevCiclical (Zipper c [] fs) = end (Zipper c [] fs)
prevCiclical (Zipper c (p:ps) fs) = Zipper p ps (c:fs)

swapPrev :: Zipper a -> Zipper a
swapPrev (Zipper c [] fs) = Zipper c [] fs
swapPrev (Zipper c (p:ps) fs) = Zipper c ps (p:fs)

swapNext :: Zipper a -> Zipper a
swapNext (Zipper c ps []) = Zipper c ps []
swapNext (Zipper c ps (f:fs)) = Zipper c (f:ps) fs

swapPrevCiclical :: Zipper a -> Zipper a
swapPrevCiclical (Zipper c [] fs) = Zipper c (reverse fs) []
swapPrevCiclical (Zipper c (p:ps) fs) = Zipper c ps (p:fs)

swapNextCiclical :: Zipper a -> Zipper a
swapNextCiclical (Zipper c ps []) = Zipper c [] (reverse ps)
swapNextCiclical (Zipper c ps (f:fs)) = Zipper c (f:ps) fs

moveToNth :: Int -> Zipper a -> Zipper a
moveToNth n = moveToNth' n . start

moveToNth' :: Int -> Zipper a -> Zipper a
moveToNth' n zipper
  | null $ following zipper = zipper
  | n <= 0 = zipper
  | otherwise = moveToNth' (n-1) $ next zipper

-- maps a functions that takes a boolean (True for the current element)
mapWithCurrent :: (Bool -> a -> b) -> Zipper a -> [b]
mapWithCurrent f zipper = (prevs ++) $ curr : foll
    where
      prevs = map (f False) . reverse $ preceding zipper
      curr = f True $ current zipper
      foll = map (f False) $ following zipper
