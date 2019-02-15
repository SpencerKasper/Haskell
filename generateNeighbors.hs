





generateNeighbors :: (Int,Int) -> [[a]] -> [(Int,Int)]
generateNeighbors (r,c) game   	  = neighbors
		  where nRows  	  = length game
		  	nCols  	  = length (head game)
			above  	  = [(row,col)|row<-[(r-1)],col<-[c],r>0]
			below  	  = [(row,col)|row<-[(r+1)],col<-[c],r<(nRows-1)]
			left   	  = [(row,col)|row<-[r],col<-[(c-1)],c>0]
			right  	  = [(row,col)|row<-[r],col<-[(c+1)],c<(nCols-1)]
			neighbors = above ++ below ++ left ++ right 