module Lab4

where

import Test.Hspec
import Test.QuickCheck
import SetOrd
import System.Random
import Data.List
import Data.List.Split
import Control.Monad

------- Exercise 3 -------
-- Automated random testing
mapSet :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapSet f (Set []) = Set []
mapSet f (Set (x:xs)) = insertSet (f x) (mapSet f (Set xs))

allPropSet :: (a -> Bool) -> Set a -> Bool
allPropSet f (Set []) = True
allPropSet f (Set (x:xs)) = f x && allPropSet f (Set xs)

-- Random int function from week 3
getRandomInt :: Int -> Int -> IO Int
getRandomInt k n = getStdRandom (randomR (k, n))

-- Creates a list of random integers of values 0 to 100 of length something between 0 and 100
randomSet :: IO (Set Int)
randomSet = do
    g <- getStdGen
    x <- getRandomInt 0 60
    return $ list2set $ take x (randomRs (0, 100) g)
    
randomSets :: Int -> IO [Set Int]
randomSets 0 = return []
randomSets n = do x <- randomSet
                  xs <- randomSets (n -1)
                  return (x:xs)
    
-- Modified test function from week 3 to accept a list of sets as input
test :: Show a => Int -> (Set a -> Set a -> Bool) -> [Set a] -> [Set a] -> IO ()
test n _ [] _ = print (show n ++ " tests passed")
test n p (s1:ss1) (s2:ss2) = 
  if p s1 s2  
  then do print ("pass on:" ++ show s1 ++ " " ++ show s2)
          test n p ss1 ss2
  else error ("failed test on:" ++ show s1 ++ show s2)

-- Now the quickCheck test
-- First, we need to make Set a an instance of Arbitrary
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
   arbitrary = sized $ \n ->
    do k <- choose (0,n)
       liftM list2set $ sequence [ arbitrary | _ <- [1..k] ]
   
-- Now we can create a testable property for Set Int
testProp :: Set Int -> Bool
testProp s@(Set x) = s == list2set x 

-- And use it for quickCheck
testSet :: IO()
testSet = quickCheck testProp

------- Exercise 4 -------
-- Union is already given in the SetOrd.hs

intersection :: (Ord a) => Set a -> Set a -> Set a 
intersection (Set []) _  =  Set []
intersection _ (Set [])  =  Set []
intersection (Set (x:xs)) set2  
        | inSet x set2 = insertSet x $ intersection (Set xs) set2
        | otherwise = intersection (Set xs) set2
        
difference :: (Ord a) => Set a -> Set a -> Set a 
difference (Set []) _  =  Set []
difference set1 (Set [])  = set1
difference (Set (x:xs)) set2  
        | inSet x set2 = difference (Set xs) set2
        | otherwise = insertSet x $ difference (Set xs) set2

-- All properties:
testProperties ::Ord a => Set a -> Set a -> Bool
testProperties s1 s2 = tpUnion s1 s2 &&
                       tpIntersection s1 s2 &&
                       tpDifference s1 s2
        
-- All elements of set 1 and all elements of set 2 should be present in the union set
tpUnion :: Ord a => Set a -> Set a -> Bool
tpUnion s1 s2 = allPropSet (== True) $ mapSet (\x -> inSet x  (unionSet s1 s2)) s1

-- Each element which is in set 1 and in set 2 should be in the difference set of s1 and s2
tpIntersection :: Ord a => Set a -> Set a -> Bool
tpIntersection s1 s2 = checkSet s1 s2 (intersection s1 s2) && checkSet s2 s1 (intersection s1 s2)
    where   
        checkSet (Set []) _ _= True
        checkSet _ (Set []) _= True
        checkSet (Set (x:xs)) s2 s3 | inSet x s2 = inSet x s3 && checkSet (Set xs) s2 s3
                                    | otherwise = checkSet (Set xs) s2 s3

tpDifference :: Ord a => Set a -> Set a -> Bool                                    
tpDifference s1 s2 = checkSet s1 s2 (difference s1 s2) && checkSet s2 s1 (difference s1 s2)
    where   
        checkSet _ _ (Set []) = True
        checkSet s1 s2 (Set (x:xs)) = not $ inSet x s2 && inSet x s1 && checkSet s1 s2 (Set xs)
  
-- Testing of the functions using the testable properties  
tests :: Int -> IO()
tests n = do
         x <- randomSets n
         y <- randomSets n 
         test n testProperties x y
 
automatedTestSets :: IO()
automatedTestSets = tests 100

-- Now using quickCheck
testSetQuick :: IO()
testSetQuick = quickCheck (testProperties :: Set Int -> Set Int -> Bool)
        
------- Exercise 5 -------
type Rel a = [(a,a)]
infixr 5 @@ 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y ==w]

exampleRel = [(1,2),(2,3),(3,4)]

-- subTrClos adds the current relation to the @@ of the relation to itself.
-- Since this can give new relations when done again it is repeated until the result is the same as the last one
trClos :: Ord a => Rel a -> Rel a
trClos r | r == trans = trans
         | otherwise = trClos trans
       where
           trans = nub $ sort $ r ++ (r@@r)

------- Exercise 6 -------
runSpec :: IO()
runSpec = hspec specTrans

specTrans :: Spec
specTrans = do
  describe "trClose (Transitive Closure)" $ do
    it "Empty Relation" $ do 
        trClos [] `shouldBe` ([] :: Rel Int)
    it "The transitive closure of a relation in transitive closure is the same list" $
        property $ \x -> trClos x == trClos (trClos (x :: Rel Int))
    it "Example relation" $ do
        trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
    it "An ordered relation with unique elements should have be the same as trClos of that relation" $
        property $ \x -> makeOrderedRelation (x :: [Int]) == trClos (makeOrderedRelation x)
     
-- Creates an ordered Relation with only unique elements
makeOrderedRelation :: [Int] -> Rel Int     
makeOrderedRelation x = makeRelation $ nub $ sort x

------- Exercise 7 -------
-- Create a random relation with at least 100 pairs and at most 200 of values in between 0 and 100\
randomRelation :: IO (Rel Int)
randomRelation = do
    g <- getStdGen
    x <- getRandomInt 100 200
    return $ makeRelation $ take x (randomRs (0, 100) g)
    
-- Creates a relation from a list
makeRelation [] = []
makeRelation [x] = []
makeRelation (x:y:xs) = (x,y) : makeRelation xs

makeList [] = []
makeList ((x,y):xs) = x:y : makeList xs

-- Use the same property as the specTrans, that the trClos a relation already showing all the transitive pairs is the same
testTransProp1 :: (Ord a, Eq a) => Rel a -> Bool
testTransProp1 r = let trans = trClos r
                   in trans == trClos trans
                   
-- The trClos of a empty list is empty
testTransProp2 :: (Ord a, Eq a) => Rel a -> Bool
testTransProp2 r | r == [] = trClos r == [] 
                 | otherwise = True -- testTransProp2 does not say anything about other list and therefore should just return True

-- The trClos of a relation with only unique elements should have the same length as the original list
testTransProp3 :: (Ord a, Eq a) => Rel a -> Bool
testTransProp3 r | list == nub list =  trClos r == r
                 | otherwise = True -- testTransProp3 does not say anything about other list and therefore should just return True
    where
        list = makeList r
      
-- All properties      
testAllTrans :: (Ord a, Eq a) => Rel a -> Bool       
testAllTrans r = testTransProp1 r &&
                 testTransProp2 r &&
                 testTransProp3 r

-- Random test generator
testTrans :: IO Bool
testTrans = randomRelation >>= return . testAllTrans
            

------- Exercise 8 -------