module Helpers
( readBattleFloodFile
, printGame
) where

import Prelude
import Data.Char
import Data.List
import Debug.Trace

readBattleFloodFile :: String -> IO ([Int],[[Char]])
readBattleFloodFile = readIO

printGame :: [[Char]] -> IO ()
printGame [] = do
	       print ""
printGame (ro:ros) = do
	  	     print ro
		     printGame ros

