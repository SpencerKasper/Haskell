import Prelude
import System.Environment ( getArgs )
import Data.List
import Helpers
import Data.Char

-- The main method that will be used for testing / comgand line access
main = do
     args <- getArgs
     filename <- readFile (head args)
     (spaces,values,game,vertical,horizontal) <- readGreaterThanSudokuFile filename
     print "Result"
     printGame (onePlayerOneMove game vertical horizontal (head spaces) (head values))

-- YOUR CODE SHOULD COME AFTER THIS POINT
onePlayerOneMove :: [[Char]] -> [[Int]] -> [[Int]] -> Int -> Int -> [[Char]]
onePlayerOneMove ga ve ho sp va
	|	   (checkSpace ga sp) 
		&& (checkRow ga sp (intToDigit va)) 
		&& (checkCol ga sp (intToDigit va))
		&& (checkSquare ga sp (intToDigit va)) 
		&& (checkVal ga ve ho sp (intToDigit va))		= place ga sp (intToDigit va)
	| otherwise								= ga
	
	

checkSpace :: [[Char]] -> Int -> Bool
checkSpace ga sp = ((peek ga sp) == '-')

checkRow :: [[Char]] -> Int -> Char -> Bool
checkRow ga sp va = 	(not ((peek ga (translateSpaceXY 0 (translateSpaceY sp))) == va))
					&&	(not ((peek ga (translateSpaceXY 1 (translateSpaceY sp))) == va))
					&&	(not ((peek ga (translateSpaceXY 2 (translateSpaceY sp))) == va))
					&&	(not ((peek ga (translateSpaceXY 3 (translateSpaceY sp))) == va))

checkCol :: [[Char]] -> Int -> Char -> Bool
checkCol ga sp va = 	(not ((peek ga (translateSpaceXY (translateSpaceX sp) 0)) == va))
					&&	(not ((peek ga (translateSpaceXY (translateSpaceX sp) 1)) == va))
					&&	(not ((peek ga (translateSpaceXY (translateSpaceX sp) 2)) == va))
					&&	(not ((peek ga (translateSpaceXY (translateSpaceX sp) 3)) == va))

checkSquare :: [[Char]] -> Int -> Char -> Bool
checkSquare ga sp va = (1==1)
--To Be Implemented

	
checkVal :: [[Char]] -> [[Int]] -> [[Int]] -> Int -> Char -> Bool
checkVal ga ve ho sp va = (checkLeft ga ho sp va)
						&& (checkRight ga ho sp va)
						&& (checkTop ga ve sp va)
						&& (checkBottom ga ve sp va)
						
						
direction :: Char -> Int
direction va
	| (va == '1')		= 1
	| (va == '4')		= -1
	| otherwise			= 0
						  
						  
checkLeft :: [[Char]] -> [[Int]] -> Int -> Char -> Bool
checkLeft ga ho sp va 
	| (translateSpaceX sp) == 0		= (1==1)
	| otherwise						= (not((peekInt ho ((translateSpaceX sp) -1) (translateSpaceY sp)) == dir))
	where dir						= direction va

checkLeft2 :: [[Char]] -> [[Int]] -> Int -> Char -> Bool
checkLeft2 ga ho sp va
	| ((peekInt ho ((translateSpaceX sp) - 1) (translateSpaceY sp)) == 1)		= (((peek ga (translateSpaceXY ((translateSpaceX sp) - 1) (translateSpaceY sp))) == '-') || ((peek ga (translateSpaceXY ((translateSpaceX sp) - 1) (translateSpaceY sp))) < (peek ga sp)))
	| otherwise																	= (((peek ga (translateSpaceXY ((translateSpaceX sp) - 1) (translateSpaceY sp))) == '-') || ((peek ga (translateSpaceXY ((translateSpaceX sp) - 1) (translateSpaceY sp))) > (peek ga sp)))

checkRight :: [[Char]] -> [[Int]] -> Int -> Char -> Bool
checkRight ga ho sp va  
	| (translateSpaceX sp) == 3		= (1==1)
	| otherwise						= (not((peekInt ho ((translateSpaceX sp)) (translateSpaceY sp)) == dir))
	where dir						= ((-1) * (direction va))

checkRight2 :: [[Char]] -> [[Int]] -> Int -> Char -> Bool
checkRight2 ga ho sp va
	| ((peekInt ho (translateSpaceX sp) (translateSpaceY sp)) == 1)		= (((peek ga (translateSpaceXY ((translateSpaceX sp) + 1) (translateSpaceY sp))) == '-') || ((peek ga (translateSpaceXY ((translateSpaceX sp) + 1) (translateSpaceY sp))) > (peek ga sp)))
	| otherwise															= (((peek ga (translateSpaceXY ((translateSpaceX sp) + 1) (translateSpaceY sp))) == '-') || ((peek ga (translateSpaceXY ((translateSpaceX sp) + 1) (translateSpaceY sp))) < (peek ga sp)))

checkTop :: [[Char]] -> [[Int]] -> Int -> Char -> Bool
checkTop ga ve sp va  
	| (translateSpaceY sp) == 0		= (1==1)
	| otherwise						= (not((peekInt ve (translateSpaceX sp) ((translateSpaceY sp) -1)) == dir))
	where dir						= direction va

checkTop2 :: [[Char]] -> [[Int]] -> Int -> Char -> Bool
checkTop2 ga ve sp va
	| ((peekInt ve (translateSpaceX sp) ((translateSpaceY sp) -1)) == 1)			= (((peek ga (translateSpaceXY (translateSpaceX sp) ((translateSpaceY sp) -1))) == '-') || ((peek ga (translateSpaceXY (translateSpaceX sp) ((translateSpaceY sp) -1))) < (peek ga sp)))
	| otherwise																		= (((peek ga (translateSpaceXY (translateSpaceX sp) ((translateSpaceY sp) -1))) == '-') || ((peek ga (translateSpaceXY (translateSpaceX sp) ((translateSpaceY sp) -1))) > (peek ga sp)))

checkBottom :: [[Char]] -> [[Int]] -> Int -> Char -> Bool
checkBottom ga ve sp va  
	| (translateSpaceY sp) == 3		= (1==1)
	| otherwise						= (not((peekInt ve ((translateSpaceX sp)) (translateSpaceY sp)) == dir)) 
	where dir						= ((-1) * (direction va))

checkBottom2 :: [[Char]] -> [[Int]] -> Int -> Char -> Bool
checkBottom2 ga ve sp va
	| ((peekInt ve (translateSpaceX sp) (translateSpaceY sp)) == 1)			= (((peek ga (translateSpaceXY (translateSpaceX sp) ((translateSpaceY sp) +1))) == '-') || ((peek ga (translateSpaceXY (translateSpaceX sp) ((translateSpaceY sp) +1))) > (peek ga sp)))
	| otherwise																= (((peek ga (translateSpaceXY (translateSpaceX sp) ((translateSpaceY sp) +1))) == '-') || ((peek ga (translateSpaceXY (translateSpaceX sp) ((translateSpaceY sp) +1))) < (peek ga sp)))


translateSpaceX ::  Int -> Int
translateSpaceX sp = (floor (((fromIntegral sp) -1)/4.0))

translateSpaceY ::  Int -> Int
translateSpaceY sp = (mod (sp -1) 4)

translateSpaceXY :: Int -> Int -> Int
translateSpaceXY x y = ((4 * x) + y) + 1

peek :: [[Char]] -> Int -> Char
peek (row:rows) sp
	| y == 0		= (peek1D row x)
	| otherwise		= (peek rows (sp -1) )
	where x     		    = (translateSpaceX sp)
	      y     		    = (translateSpaceY sp)
		  
	
peek1D :: [Char] -> Int -> Char
peek1D (col:cols) x
	| x == 0		= col
	| otherwise		= peek1D cols (x-1)
	
	
peekInt :: [[Int]] -> Int -> Int -> Int
peekInt (row:rows) x y
	| y == 0		= (peekInt1D row x)
	| otherwise		= (peekInt rows x (y-1))

		  
	
peekInt1D :: [Int] -> Int -> Int
peekInt1D (col:cols) x
	| x == 0		= col
	| otherwise		= peekInt1D cols (x-1)


place :: [[Char]] -> Int -> Char -> [[Char]]
place (row:rows) sp va
	| y == 0		= (place1D row x va):rows
	| otherwise		= (row:(place rows (sp -1) va))
	where x		       		= (translateSpaceX sp)
	      y         		= (translateSpaceY sp)
	
	
place1D :: [Char] -> Int -> Char -> [Char]
place1D (col:cols) x va
	| x == 0		= va:cols
	| otherwise		= col:(place1D cols (x-1) va)