module Lab3

where

import Week3

---- Exercise 1 ----
-- More example forms for us to check if base cases are True
form4 = Impl p q
form5 = Impl (Neg q) (Neg p)

-- Opposite of all which checks if the result of function f on all the elements of the list return False
none :: (a -> Bool) -> [a] -> Bool
none _ [] = True
none f (x:xs) = not (f x) && none f xs

-- Check if none of the evaluated form returns True
-- We used none instead of not . satisfiable since we can now use not . satisfiable as a testing proeprty
contradiction :: Form -> Bool
contradiction f = none (\ v -> eval v f) (allVals f)

-- Check if all of the evaluated form returns True
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- Logical equivalent means that when f1 is true, f2 should be true as well, thus having f1 imply f2
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- Check if f1 and f2 are give always have the same truth value
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

--- Tests of formulas above
-- If a formula is a tautology it is not a contradiction and vice versa
-- If a formula is a contradiction is not satisfiable and vice versa
-- A formula should be a contradiction, tautology or satisfiable
testValidity :: Form -> Bool
testValidity f  | tautology f = not (contradiction f) && satisfiable f
                | contradiction f = not (tautology f) && not (satisfiable f)
                | satisfiable f = not (contradiction f)
                | otherwise = False
 
-- If two formulas are equivalent they should imply each other
-- If not the the formulas are not equivalent this test should just return True since
-- non equivalent functions are not tested here
testEquiv :: Form -> Form -> Bool 
testEquiv f1 f2| equiv f1 f2 = entails f1 f2 && entails f2 f1
               | otherwise = True
               
--testEntails :: Form -> Form -> Bool
--testEntails f1 f2 | entails f1 f2 = (\ v -> eval v f) (allVals f1)

---- Exercise 2 ----
cnf :: Form -> Form
cnf form = f $ (nnf . arrowfree) form -- Use function f on nnf of form
    where 
        -- Pattern match the sort of form in f, Prop, Neg and Cnj are already in proper form
        f (Prop p) = Prop p
        f (Neg p) = Neg (f p)
        f (Cnj fs) = Cnj (map f fs)
        f (Dsj fs) = dist (map f fs)

        -- sub function dist which expects a list of forms and does the DIST algorithm
        -- from the lecture on each of the elements
        dist [] = Dsj []
        dist (x:xs) = foldr subDist x xs

        -- the DIST algorithm from the lecture, only make it useful for conjunctions as list instead of a pair
        subDist (Cnj f) p = Cnj (map (\x -> subDist x p) f)    
        subDist f (Cnj p) = Cnj (map (\x -> subDist f x) p)
        subDist f1 f2 = Dsj [f1, f2]
        
---- Exercise 3 ----
--- Testable properties are postconditions of a CNF
allProps, propEquiv, propArrowFree, propNNF, propCNF :: Form -> Bool

-- A CNF of a formula should be logically equivalent to its original formula
propEquiv f = equiv (cnf f) f

-- A CNF of a formula should be logically equivalent to the arrow free formula
propArrowFree f = equiv (arrowfree f) f

-- A CNF of a formula should be logically equivalent to the nnf formula
-- With the pre condition that f should already be in arrowfree form
propNNF f = let a = arrowfree f 
            in equiv (nnf a) f 

-- A CNF of a formula should be logically equivalent to the CNF of the CNF of the formula
propCNF f = let c = cnf f 
            in equiv (cnf c) c
            
allProps f = propEquiv f &&
             propArrowFree f &&
             propNNF f &&
             propCNF f

--- Random test generator
-- testGenerator will generate 50 random formulas and check for each of them
-- if the testProperties
testGenerator :: IO()
testGenerator = do
                formulas <- (getRandomFs 50)
                test 70 allProps formulas
                
---- Exercise 4 ----
type Clause = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj f) = undefined
cnf2cls (Dsj f) = [concatDisjuncts f]
cnf2cls (Neg p) = map (map (*(-1))) (cnf2cls p)
cnf2cls (Prop p) = [[p]]

concatDisjuncts [] = []
concatDisjuncts (Dsj x:xs) = concatDisjuncts x ++ concatDisjuncts xs
concatDisjuncts (Prop x:xs) = x : concatDisjuncts xs
concatDisjuncts (Neg (Prop x):xs) = (-x) : concatDisjuncts xs
