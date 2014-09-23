module Lab3

where

import Week3

---- Exercise 1 ----
-- Opposite of all which checks if the result of function f on all the elements of the list return False
none :: (a -> Bool) -> [a] -> Bool
none _ [] = True
none f (x:xs) = not (f x) && none f xs

-- Check if none of the evaluated form returns True
-- We used none instead of not . satisfiable since we can now use not . satisfiable as a testing property
contradiction :: Form -> Bool
contradiction f = none (\ v -> eval v f) (allVals f)

-- Check if all of the evaluated form returns True
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- Logical equivalent means that when f1 is true, f2 should be true as well, thus having f1 imply f2 for all evaluations of f1 and f2
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- Check if f1 and f2 are give always have the same truth value
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

--- Tests of formulas above
-- If a formula is a contradiction is not satisfiable and vice versa
-- A formula should be a contradiction, tautology or satisfiable
testValidity :: Form -> Bool
testValidity f  -- If a formula is a tautology it is not a contradiction and vice versa
                | tautology f = not (contradiction f) && satisfiable f
                | contradiction f = not (tautology f) && not (satisfiable f)
                
                -- Satisfiable means no contradiction
                | satisfiable f = not (contradiction f)
                
                -- It should always be one of the above cases
                | otherwise = False

-- apply lets you apply a function to a list to return a list of Bools
apply :: (a -> Bool) -> [a] -> [Bool]
apply _ [] = []
apply f (x:xs) = (f x) : apply f xs

-- Use apply to get a list of all evaluation values of a form
evalAll :: Form -> [Bool]
evalAll f = apply (\ v -> eval v f) (allVals f)

-- This test functions checks if you zip all valuations of both formulas if (not f1 || f2) is true for all elements of the list
testImpl :: Form -> Form -> Bool
testImpl f1 f2 | entails f1 f2 =  let table = zip (evalAll f1) (evalAll f2)
                                  in all (\(x,y) -> not x || y) table
               | otherwise = True
                 
-- If two formulas are equivalent they should imply each other. If not the the formulas are not equivalent this test 
-- should just return True since non equivalent functions are not tested here
testEquiv :: Form -> Form -> Bool 
testEquiv f1 f2| equiv f1 f2 = entails f1 f2 && entails f2 f1
               | otherwise = True
     
-- Random testing of formulas
testAllFormulas :: IO()
testAllFormulas = do
                formulas <- (getRandomFs 100)
                test 100 testValidity formulas

---- Exercise 2 ----
cnf :: Form -> Form
cnf form = f $ (nnf . arrowfree) form -- Use function f on nnf of form
    where 
        -- Pattern match the sort of form in f, Prop, Neg and Cnj are already in proper form
        f (Prop p) = Prop p
        f (Neg p) = Neg (f p)
        f (Cnj fs) = Cnj (map f fs)
        f (Dsj fs) = dist (map f fs)

        -- Sub function dist which expects a list of forms and does the DIST algorithm from the lecture on each of the elements
        dist [] = Dsj []
        dist (x:xs) = foldr subDist x xs

        -- The DIST algorithm from the lecture, only it is made useful for conjunctions as a list instead of a pair
        -- Since conjunctions can be nested on each of the elements in the conjunctions the subDist should be applied
        subDist (Cnj f) p = Cnj (map (\x -> subDist x p) f)    
        subDist f (Cnj p) = Cnj (map (\x -> subDist f x) p)
        subDist f1 f2 = Dsj [f1, f2]
        
---- Exercise 3 ----
--- Testable properties are postconditions of a CNF
allProps, propEquiv, propArrowFree, propNNF, propCNF :: Form -> Bool

-- A CNF of a formula should be logically equivalent to its original formula
propEquiv f = equiv (cnf f) f

-- A formula should be logically equivalent to the arrow free formula
propArrowFree f = equiv (arrowfree f) f

-- A CNF of a formula should be logically equivalent to the NNF formula, with the pre condition that f should already be in arrowfree form
propNNF f = equiv (nnf (arrowfree f )) (cnf f) 

-- A CNF of a formula should be logically equivalent to the CNF of the CNF of the formula
propCNF f = let c = cnf f 
            in equiv (cnf c) c
            
allProps f = propEquiv f &&
             propArrowFree f &&
             propNNF f &&
             propCNF f

-- Random test generator
-- TestGenerator will generate 100 random formulas and check for each of them if the testProperties hold
testGenerator :: IO()
testGenerator = do
                formulas <- (getRandomFs 100)
                test 100 allProps formulas
                
---- Exercise 4 ----
type Clause = [Int]
type Clauses = [Clause]

-- Seems to give the right result, though with some propositions in the wrong order. This however makes logically no difference since
-- order does not matter in disjunctions. 
cnf2cls :: Form -> Clauses
cnf2cls form = cnf2cls' form 
    where
        -- Each conjunction is a clause over which the cnf2cls' should be executed, the results can be concatenated so all conjunctions are
        -- at the same level (instead of nested, which makes no difference logically seen)
        cnf2cls' (Cnj f) = concat $ map cnf2cls' f
        
        -- Apply sub function to all disjunctions to create a clause
        cnf2cls' (Dsj f) = [concatDisjuncts f]
        
        -- Negate every every proposition letter in the cls (no swapping of conjuncts and disjuncts since negations only negate one proposition each
        cnf2cls' (Neg p) = map (map (*(-1))) (cnf2cls' p)
        
        -- One clause
        cnf2cls' (Prop p) = [[p]]
        
        -- Sub function that creates one clause for each disjunct given. Disjunctions are nested  in the CNF which makes logically no 
        -- difference and therefore are put at the same level to create one clause.
        concatDisjuncts :: [Form] -> Clause
        concatDisjuncts [] = []
        concatDisjuncts (Dsj x:xs) = concatDisjuncts x ++ concatDisjuncts xs
        concatDisjuncts (Prop x:xs) = x : concatDisjuncts xs
        concatDisjuncts (Neg (Prop x):xs) = (-x) : concatDisjuncts xs

-- Test the CLS function with the clsProp
testC2C :: IO()
testC2C = do
          formulas <- (getRandomFs 100)
          test 100 clsProp formulas

-- Some help functions to evaluate forms and cls
-- Create a disjunction form of a clause
clauseToForm :: Clause -> Form
clauseToForm xs = Dsj $ map clauseToProp xs
    where
        clauseToProp x | x > 0 = Prop x
                       | otherwise = Neg (Prop (x * (-1)))
            
-- Expects a form, create a cls from it and thereafter put it again in Form format            
cls2form :: Form -> Form
cls2form f = let c = cnf f
             in Cnj $ map clauseToForm (cnf2cls c) -- cls is a conjunction of disjunctions but in another format
             
-- To check if the above function actually gives the expected result, check if the cls2form to a cls gives the same as form to a cls
checkcls2form :: Form -> Bool
checkcls2form f = cnf2cls (cnf f) == cnf2cls (cls2form f)
             
-- A the form of a cls should be logically equivalent the the original form, as it should be to the nnf, arrowfree and cnf of that form
clsProp :: Form -> Bool
clsProp f = let clsf = cls2form f
            in equiv f clsf &&
               equiv (arrowfree f) clsf &&
               equiv ((nnf.arrowfree) f) clsf &&
               equiv (cnf f) clsf 