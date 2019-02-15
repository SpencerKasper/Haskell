




get :: [[a]] -> (Int,Int) -> a
-- get game position = element at that position in the game
get (row:_) (0,c)    = getCol row c
get (_:rows) (r,c)   = get rows ((r-1),c)



getCol :: [a] -> Int -> a
-- getCol row position = element at that position in the row
getCol (col:_) 0       = col
getCol (_:cols) c      = getCol cols (c-1)