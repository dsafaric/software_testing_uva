-- Lab5.hs Exercise

module Lab5

where

import Week5
import Data.List
import Test.Hspec
import System.Random
import Test.QuickCheck
import System.IO.Unsafe

-- Exercise 1
-- get the constraints function, and uniqueSolv 
-- you basically need to write a sudoko consistency check using Hspec
-- the sudoko should always have unique solution (only one solution) and should always be consistent

cons :: IO ()
cons = hspec $ do
	describe "consistent" $ do
		it "sudoko is consistent" $ do
			consistent (grid2sud example1) `shouldBe` (True :: Bool)
	describe "unique" $ do
		it "sudoko solution is unique" $ do
			uniqueSol (nonIO genRandomSudoku) `shouldBe` (True :: Bool)
		-- should we use generated and solved Sudoku's or the unsolved ones?
		-- in case of an solved one, we can use genRandomSudoku, while in the other case
		-- we use genSud which generates a solved sudoku and generates a problems of it

genSud :: IO Node
genSud = do
		[r] <- rsolveNs [emptyN]
		s <- genProblem r
		return s

nonIO :: IO a -> a
nonIO = \x -> unsafePerformIO x

