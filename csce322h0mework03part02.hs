import Prelude
import System.Environment ( getArgs )
import Data.List
import Helpers

-- The main method that will be used for testing / comgand line access
main = do
     args <- getArgs
     filename <- readFile (head args)
     (moves,game) <- readBattleFloodFile filename
     print "Result"
     printGame (onePlayersManyMoves game moves "1")

-- YOUR CODE SHOULD COME AFTER THIS POINT
onePlayersManyMoves :: [[Char]] -> [Int] -> [Char] -> [[Char]]
onePlayersManyMoves game [] _                = game
onePlayersManyMoves game (move:moves) player = onePlayersManyMoves (changeCluster game (findCluster game [] [(0,0)] move 1) (head (player))) moves (incrementPlayer player)

incrementPlayer :: [Char] -> [Char]
incrementPlayer (h:t) = (t ++ [h])


findCluster :: [[Char]] -> [(Int, Int)] -> [(Int, Int)]  -> Int -> Int -> [(Int, Int)]
-- game checkedPositions currentPosition move counter = positions representing cluster number of move 
findCluster game checkedPositions currentPosition move counter
		| ((move*2) == counter) || ((move*2-1) == counter) = findNeighbors game currentPosition checkedPositions
		| ((move*2) < counter) || ((move*2-1) < counter) = []
		| otherwise             = findCluster game (removeDuplicates (checkedPositions ++ (findNeighbors game currentPosition checkedPositions))) (removeDuplicates passingCurrent) move (counter+1)
		where newPosition       = [n|n<-(generatePositions game),(not (elem n checkedPositions))]
		      passingCurrent    = [x|x<-newPosition, (x == (head newPosition))]

changeCluster :: [[Char]] -> [(Int, Int)] -> Char -> [[Char]]
-- game clusterPositions player = game with changed cluster to player
changeCluster game [] _ = game
changeCluster game (h:clusters) player = (changeCluster (set game h player) clusters player)

-- Code written together in class that is copied and pasted from the website...as I understand we are allowed to use this
findNeighbors :: [[Char]] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
-- findNeighbors game positionsToCheck positionsChecked
findNeighbors _ [] _ = []
findNeighbors game (position:positions) checked = position:(findNeighbors game (positions++usefulNeighbors) (position:checked))
	    where positionNeighbors = generateNeighbors position game
	      	  usefulNeighbors   = [n|n<-positionNeighbors,(get game n)==(get game position),(not (elem n positions)),(not (elem n checked))]	

generatePositions :: [[a]] -> [(Int,Int)]
-- generate all positions for a given game
-- 8x7 game should generate (0,0), (0,1), ..., (0,6), (1,0), ..., (7,6)
generatePositions game = positions
		  where nRows		= length game
		  	nCols 		= length (head game)
			positions 	= [(r,c)|r<-[0..(nRows-1)],c<-[0..(nCols-1)]]			  

generateNeighbors :: (Int,Int) -> [[a]] -> [(Int,Int)]
generateNeighbors (r,c) game   	  = neighbors
		where nRows       = length game
		      nCols       = length (head game)
		      above       = [(row,col)|row<-[(r-1)],col<-[c],r>0]
		      below       = [(row,col)|row<-[(r+1)],col<-[c],r<(nRows-1)]
		      left        = [(row,col)|row<-[r],col<-[(c-1)],c>0]
		      right       = [(row,col)|row<-[r],col<-[(c+1)],c<(nCols-1)]
		      neighbors   = above ++ below ++ left ++ right 
	

removeDuplicates :: Ord a => [a] -> [a]
-- remove duplicates from a list
-- abc
removeDuplicates [] 	= []
removeDuplicates [x] 	= [x]
removeDuplicates (h:t)
	| h == n    = removeDuplicates (h:ta)
	| otherwise	= h:(removeDuplicates t)
	where   n   = head t
	        ta  = tail t
	
	
set :: [[a]] -> (Int,Int) -> a -> [[a]]
-- set game position element = game with position replaced by element
set (row:rows) (0,c) el = (setCol row c el):rows
set (row:rows) (r,c) el = row:(set rows (r-1,c) el)

setCol :: [a] -> Int -> a -> [a]
setCol (_:cols) 0 el 	= el:cols
setCol (col:cols) c el	= col:(setCol cols (c-1) el)


get :: [[a]] -> (Int,Int) -> a
-- get game position = element at that position in the game
get (row:_) (0,c)    = getCol row c
get (_:rows) (r,c)   = get rows ((r-1),c)

getCol :: [a] -> Int -> a
-- getCol row position = element at that position in the row
getCol (col:_) 0       = col
getCol (_:cols) c      = getCol cols (c-1)