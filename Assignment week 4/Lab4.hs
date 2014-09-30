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
-- Random int function from week 3
getRandomInt :: Int -> Int -> IO Int
getRandomInt k n = getStdRandom (randomR (k, n))

-- Creates a list of random integers of values 0 to 100 of length something between -1000 and 1000
randomSet :: IO (Set Int)
randomSet = do
    g <- getStdGen
    x <- getRandomInt 0 100
    return $ list2set $ take x (randomRs (-1000, 1000) g)
    
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

---- Since we each did the assignments, this is another solution we came up with: -----
-- Union, Intersection and Difference functions by using the following approach:
    -- Set property -> List [a]
    -- processing List [a] -> returning (Set a) property

set2list :: Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = x : set2list (Set xs)

union' :: Ord a => Set a -> Set a -> Set a
union' xs ys = list2set (nub (set2list xs ++ set2list ys))

intersect' :: Ord a => Set a -> Set a -> Set a
intersect' xs ys = list2set ((set2list xs) `intersect` (set2list ys))

-- All properties:
testProperties ::Ord a => Set a -> Set a -> Bool
testProperties s1 s2 = tpUnion s1 s2 &&
                       tpUnion2 s1 s2 && 
                       tpIntersection s1 s2 &&
                       tpIntersection2 s1 s2 &&
                       tpDifference s1 s2
        
-- All elements of set 1 and all elements of set 2 should be present in the union set
tpUnion :: Ord a => Set a -> Set a -> Bool
tpUnion s1 s2 = checkSet s1 u && checkSet s2 u
    where
        u = unionSet s1 s2
        checkSet (Set []) _ = True
        checkSet (Set (x:xs)) s2 | inSet x s2 = checkSet (Set xs) s2
                                 | otherwise = False
                 
-- Only elements in s1 and s2 should be in the union set              
tpUnion2 :: Ord a => Set a -> Set a -> Bool
tpUnion2 s1 s2 = checkSet s1 s2 u
    where
        u = unionSet s1 s2
        checkSet _ _ (Set []) = True
        checkSet s1 s2 (Set (x:xs)) | inSet x s1 || inSet x s2 = checkSet s1 s2 (Set xs)
                                    | otherwise = False

-- Each element which is in set 1 and in set 2 should be in the intersection set of s1 and s2
tpIntersection :: Ord a => Set a -> Set a -> Bool
tpIntersection s1 s2 = checkSet s1 s2 i && checkSet s2 s1 i
    where   
        i = intersection s1 s2
        checkSet (Set []) _ _= True
        checkSet (Set (x:xs)) s2 s3 | inSet x s2 = inSet x s3 && checkSet (Set xs) s2 s3
                                    | otherwise = checkSet (Set xs) s2 s3
  
-- Only elements in s1 and s2 should in the intersection set.\  
tpIntersection2 :: Ord a => Set a -> Set a -> Bool
tpIntersection2 s1 s2 = checkSet s1 s2 i
    where   
        i = intersection s1 s2
        checkSet _ _ (Set [])= True
        checkSet s1 s2 (Set (x:xs)) | inSet x s1 && inSet x s2 = checkSet s1 s2 (Set xs)
                                    | otherwise = False
                                    


tpDifference :: Ord a => Set a -> Set a -> Bool                                    
tpDifference s1 s2 = checkSet s1 s2 d && checkSet s2 s1 d
    where   
        d = difference s1 s2
        checkSet _ _ (Set []) = True
        checkSet s1 s2 (Set (x:xs)) = not $ inSet x s2 && inSet x s1 && checkSet s1 s2 (Set xs)
       
-- Test properties of the functions given above  
      
testProp2 :: Set Int -> Set Int -> Bool
testProp2 s1@(Set x) s2@(Set y) = (s1 `difference` s2) == list2set (x \\ y)  -- 100 passed

testProp3 :: Set Int -> Set Int -> Bool
testProp3 s1@(Set x) s2@(Set y) = (s1 `union'` s2) == list2set (x `union` y) -- 100 passed

testProp4 :: Set Int -> Set Int -> Bool
testProp4 s1@(Set x) s2@(Set y) = (s1 `intersection'` s2) == list2set (x `intersect` y) -- 100 passed
  
-- Testing of the functions using the testable properties  
tests :: Int -> IO()
tests n = do
         x <- randomSets n
         y <- randomSets n 
         test n testProperties x y
 
automatedTS :: IO()
automatedTS = tests 100

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
    it "Single Relation" $ do
        property $ \x -> ([(x,x)] :: [(Int, Int)]) == trClos [(x,x)]
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
makeRelation :: [a] -> Rel a
makeRelation [] = []
makeRelation [x] = []
makeRelation (x:y:xs) = (x,y) : makeRelation xs

-- Makes a list from a relation
makeList :: Rel a -> [a]
makeList [] = []
makeList ((x,y):xs) = x:y : makeList xs

-- Use the same property as the specTrans, that the trClos a relation already showing all the transitive pairs is the same
testTransProp1 :: (Eq a, Ord a) => Rel a -> Bool
testTransProp1 r = let trans = trClos r
                   in trans == trClos trans
                   
-- The trClos of a empty list is empty
testTransProp2 :: (Eq a, Ord a) => Rel a -> Bool
testTransProp2 r | r == [] = trClos r == [] 
                 | otherwise = True -- testTransProp2 does not say anything about other list and therefore should just return True

-- The trClos of a relation with only unique elements should have the same length as the original list
testTransProp3 :: (Eq a, Ord a) => Rel a -> Bool
testTransProp3 r | list == nub list =  trClos r == sort r
                 | otherwise = True -- testTransProp3 does not say anything about other list and therefore should just return True
    where
        list = makeList r
      
-- All properties      
testAllTrans :: (Ord a, Eq a) => Rel a -> Bool       
testAllTrans r = testTransProp1 r &&
                 testTransProp2 r &&
                 testTransProp3 r

-- Random test generator for one 
testTrans :: IO Bool
testTrans = randomRelation >>= return . testAllTrans

-- Rel a gives problems as an instance of Arbitrary, so we need to make it explicit what type a is..
-- In this case, chosen for Int
testTrClos :: IO()
testTrClos = quickCheck (testAllTrans :: Rel Int -> Bool)

------- Exercise 8 -------
-- See explanation in the report 
fp :: Eq a => (a -> a) -> a -> a
fp f = \x -> if x == f x then x else  (f x)

-- Example function
s = (\x -> (x + 4/x)/2)
