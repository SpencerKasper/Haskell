




set :: [[a]] -> (Int,Int) -> a -> [[a]]
-- set game position element = game with position replaced by element
set (row:rows) (0,c) el = (setCol row c el):rows
set (row:rows) (r,c) el = row:(set rows (r-1,c) el)

setCol :: [a] -> Int -> a -> [a]
setCol (_:cols) 0 el 	= el:cols
setCol (col:cols) c el	= col:(setCol cols (c-1) el)
