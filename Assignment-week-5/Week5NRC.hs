-- Week5Modified.hs

module Week5Modified

where

import Week5
import Data.List
import System.Random
import Control.Monad

-- an example of a NRC Sudoku
example1Grid :: Grid
example1Grid = [[0,0,2,0,0,0,0,0,0],
				[0,0,4,0,8,0,9,0,0],
				[0,0,0,3,0,0,0,0,0],
				[0,0,0,0,0,5,4,0,1],
				[0,0,0,0,0,0,0,0,0],
				[5,0,0,2,0,0,0,0,8],
				[0,0,0,0,0,6,0,7,0],
				[0,5,0,0,0,0,0,0,0],
				[0,0,0,0,3,0,0,1,0]]

blocks' :: [[Int]]
blocks' = [[2..4],[6..8]]

-- create a new showRow function, showRowBetween function and showGrid 
	--which combines showRow and showRowBetween

showRowU :: [Value] -> IO()
showRowU [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr "  "
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr " "
     putStr (showVal a5) ; putChar ' '
     putStr "  "
     putStr (showVal a6) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr "  "
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showRowBetween = do putStrLn "     +-----|--+ +--|-----+ "
showRow' [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
	do 	putChar '|'					; putChar ' '
		putStr (showVal a1)			; putChar ' '
		putChar '|'					; putChar ' '
		putStr (showVal a2)			; putChar ' '
		putStr (showVal a3)			; putChar ' '
		putChar '|'					; putChar ' '
		putStr (showVal a4)			; 
		putChar '|'					; putChar ' '
		putStr (showVal a5)			; putChar ' '
		putChar '|'					; putChar ' '
		putStr (showVal a6)			; 
		putChar '|'					; putChar ' '
		putStr (showVal a7)			; putChar ' '
		putStr (showVal a8)			; putChar ' '
		putChar '|'					; putChar ' '
		putStr (showVal a9)			; putChar ' '
		putChar '|'					; putChar '\n'

showGrid' :: Grid -> IO()
showGrid' [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+---------+---------+---------+")
    showRowU as; showRowBetween; showRow' bs; showRow' cs
    putStrLn ("+---------+---------+---------+")
    showRow' ds; showRowBetween; showRowU es; showRowBetween; showRow' fs
    putStrLn ("+---------+---------+---------+")
    showRow' gs; showRow' hs; showRowBetween; showRowU is
    putStrLn ("+---------+---------+---------+")




