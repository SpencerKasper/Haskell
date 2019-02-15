




generatePositions :: [[a]] -> [(Int,Int)]
-- generate all positions for a given game
-- 8x7 game should generate (0,0), (0,1), ..., (0,6), (1,0), ..., (7,6)
generatePositions game = positions
		  where nRows		= length game
		  	nCols 		= length (head game)
			positions 	= [(r,c)|r<-[0..(nRows-1)],c<-[0..(nCols-1)]]