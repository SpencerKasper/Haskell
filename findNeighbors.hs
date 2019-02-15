import Data.List

findNeighbors :: [[Char]] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
-- findNeighbors game positionsToCheck positionsChecked
findNeighbors _ [] _ = []
findNeighbors game (position:positions) checked = position:(findNeighbors game (positions++usefulNeighbors) (position:checked))
	      where positionNeighbors = generateNeighbors position game
	      	    usefulNeighbors   = [n|n<-positionNeighbors,(get game n)==(get game position),(not (elem n positions)),(not (elem n checked))]	      

generateNeighbors :: (Int,Int) -> [[a]] -> [(Int,Int)]
generateNeighbors (r,c) game   	  = neighbors
		  where nRows  	  = length game
		  	nCols  	  = length (head game)
			above  	  = [(row,col)|row<-[(r-1)],col<-[c],r>0]
			below  	  = [(row,col)|row<-[(r+1)],col<-[c],r<(nRows-1)]
			left   	  = [(row,col)|row<-[r],col<-[(c-1)],c>0]
			right  	  = [(row,col)|row<-[r],col<-[(c+1)],c<(nCols-1)]
			neighbors = above ++ below ++ left ++ right 

get :: [[a]] -> (Int,Int) -> a
-- get game position = element at that position in the game
get (row:_) (0,c)    = getCol row c
get (_:rows) (r,c)   = get rows ((r-1),c)



getCol :: [a] -> Int -> a
-- getCol row position = element at that position in the row
getCol (col:_) 0       = col
getCol (_:cols) c      = getCol cols (c-1)