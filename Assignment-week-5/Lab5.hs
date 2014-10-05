-- Lab5.hs Exercise

module Lab5

where

import Week5
import Data.List
import Test.Hspec
import System.Random
import Test.QuickCheck
import System.IO.Unsafe
import SetOrd
-- Exercise 1
-- get the constraints function, and uniqueSolv 
-- you basically need to write a sudoko consistency check using HSpec
-- the sudoko should always have unique solution (only one solution) and should always be consistent


cons :: IO ()
cons = hspec $ do
	describe "sudoko" $ do
		it "sudoko is consistent - it has all unique values" $ do
			r <- genRandomSudoku
			((consistent . fst) r) `shouldBe` (True :: Bool)
		it "sudoko has no zeros left" $ do
			r <- genRandomSudoku
			length (filter (==True) (filterZeros ((constraints . fst) r ))) `shouldBe` (0 :: Int)
		it "sudoko is minimal - has only one solution" $ do
			r <- genRandomSudoku
			uniqueSol r `shouldBe` (True :: Bool)
			

genSud :: IO Node
genSud = do
		[r] <- rsolveNs [emptyN]
		s <- genProblem r
		return s

nonIO :: IO a -> a
nonIO = \x -> unsafePerformIO x

-- the filterZeros function takes a list of Contraints given from a Sudoko type, and checks
	-- whether the values of constraints contain zeros. If so, then it appends to the produced
	-- list a True (:: Bool) value, and by getting the length of filtered true values we can
	-- tell if it the random generated Sudoku contains any zero values 

filterZeros :: [Constraint] -> [Bool]
filterZeros [] = []
filterZeros (x:xs) = helper x : filterZeros xs
	where helper (_,_,x) 	| head x == 0	= True
							| otherwise		= False

-- the following function can be used for testing so that cons won't test on the basis
	-- of one sample only

genRandomSudokus :: Int -> IO [Node]
genRandomSudokus 0 = return []
genRandomSudokus n = do
		x <- genRandomSudoku
		xs <- genRandomSudokus (n-1)
		return (x:xs)

-- Exercise 2
-- every Sudoku problem P is minimal if it admits a unique solution

-- solution 1
minimal :: IO Node -> Bool
minimal = uniqueSol . nonIO 	-- the function uses nonIO (IO Node) because of we want to test
								-- the property on a random generated Sudoku problem
								-- we are using the genSud function, althouth the predifined
								-- genRandomSudoku could be used as well
-- the tests are returning a true value of every input 

-- solution 2

randInt :: Int -> Int -> IO Int
randInt = \x -> \y -> getStdRandom (randomR (x,y))

randList :: IO [Int]
randList = do 
	g <- getStdGen
	x <- randInt 10 20
	return $ take x (randomRs (1,9) g)

randCons :: [Int] -> [(Row, Column)]
randCons [] = []
randCons [x] = []
randCons (x:y:xs) = (x,y) : randCons xs

randCons' :: [(Row,Column)]
randCons' = nub $ randCons (nonIO randList)

-- node that was generated

minimalHspec :: Node -> [(Row, Column)] -> IO ()
minimalHspec n xs = hspec $ do 
	describe "uniqueSol" $ do
		it "Node has a unique solution"  $ do
			(uniqueSol $ minimalize n xs) `shouldBe` (True :: Bool) 


testProp :: IO ()
testProp = minimalHspec (nonIO genRandomSudoku) randCons'
-- the idea was to either use a list of nodes or a list of constraints to minimalize nodes to a level of 
	-- minimalness.. But that unfortunally doesn't work with Hspec since applying it to a list
	-- of objects is difficult because it accepts only the Testable objects -  while Sudoko isn't

-- Exercise 3
-- create a random sudoku generator that generates a Sudoku with three empty blocks
	-- steps:
	-- generate a sudoku problem
	-- repeat the process of deleting blocks three times
		-- the random block number is a random Int between 1 and 9
		-- find the block
		-- place all zeros in the block 

-- use the eraseS function :: Sudoku -> (Row,Column) -> Sudoku

sqrtSize :: Int
sqrtSize = 3 -- size of the blocks

size :: Int
size = (*) sqrtSize sqrtSize

blocks' :: [[(Row,Column)]]
blocks' = [[(x + i , y +j ) | i <- [1..sqrtSize], j <- [1..sqrtSize]]
		| 	x <- [0, sqrtSize..size-sqrtSize],
			y <- [0, sqrtSize..size-sqrtSize]]

getBlock :: [(Row,Column)]
getBlock = let r = 1 `randInt` 9 in blocks' !! (nonIO r - 1)

deleteBlocks :: Sudoku -> [(Row,Column)] -> Sudoku
deleteBlocks s ((r,c):xs) = deleteBlock s (r,c) : deleteBlocks s xs 

deleteBlock :: Sudoku -> (Row,Column) -> Sudoku
deleteBlock s (r,c) = eraseS s (r,c)
