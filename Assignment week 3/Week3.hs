-- The same Week3 module as given in the assignment, only with comments for easier understanding for us

module Week3 where 

import Data.List
import Data.Char
import System.Random

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving Eq

instance Show Form where 
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f 
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

p = Prop 1
q = Prop 2
r = Prop 3

form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

propNames :: Form -> [Name]
propNames = sort.nub.pnames where 
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concat (map pnames fs)
  pnames (Dsj fs) = concat (map pnames fs)
  pnames (Impl f1 f2) = concat (map pnames [f1,f2])
  pnames (Equiv f1 f2) = 
          concat (map pnames [f1,f2])

type Valuation = [(Name,Bool)]

-- all possible valuations for list of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) = 
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

eval :: Valuation -> Form -> Bool
eval [] (Prop c)    = error ("no info: " ++ show c)
eval ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = eval xs (Prop c)
eval xs (Neg f)  = not (eval xs f)
eval xs (Cnj fs) = all (eval xs) fs
eval xs (Dsj fs) = any (eval xs) fs
eval xs (Impl f1 f2) = 
     not (eval xs f1) || eval xs f2
eval xs (Equiv f1 f2) = eval xs f1 == eval xs f2

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> eval v f) (allVals f)

data Token 
      = TokenNeg
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv 
      | TokenInt Int 
      | TokenOP
      | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs) 
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs 
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseForm :: Parser Token Form 
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) = 
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) = 
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) = 
  [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                         (f2,rest) <- parseImpl ys ]
   ++
  [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                          (f2,rest) <- parseEquiv ys ] 
parseForm tokens = []

parseForms :: Parser Token [Form] 
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens = 
   [(f:fs, rest) | (f,ys) <- parseForm tokens, 
                   (fs,rest) <- parseForms ys ]

parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseImpl tokens = []

parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseEquiv tokens = []

parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

-- Random int in range {0,..,n}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- Return one random Form
getRandomF :: IO Form
getRandomF = do d <- getRandomInt 4
                getRndF d

-- Return a random propositional letter
getRndF :: Int -> IO Form 
getRndF 0 = do m <- getRandomInt 20
               return (Prop (m+1))

-- Formulas with complexity > 0
getRndF d = do n <- getRandomInt 5
               case n of 
                  0 -> do m <- getRandomInt 20
                          return (Prop (m+1))
                  1 -> do f <- getRndF (d-1)
                          return (Neg f) 
                  2 -> do m  <- getRandomInt 5 
                          fs <- getRndFs (d-1) m
                          return (Cnj fs)
                  3 -> do m  <- getRandomInt 5 
                          fs <- getRndFs (d-1) m
                          return (Dsj fs)
                  4 -> do f <- getRndF (d-1)
                          g <- getRndF (d-1)
                          return (Impl f g) 
                  5 -> do f <- getRndF (d-1)
                          g <- getRndF (d-1)
                          return (Equiv f g) 

-- Get a list of random formulas
getRandomFs :: Int ->  IO [Form]
getRandomFs n = do d <- getRandomInt 3
                   getRndFs d n     

-- Get a list of random formlas of complexity d 
getRndFs :: Int -> Int -> IO [Form]
getRndFs _ 0 = return []
getRndFs d n = do f <- getRndF d
                  fs <- getRndFs d (n-1) 
                  return (f:fs)

-- Test n formulas using propery p
test :: Int -> (Form -> Bool) -> [Form] -> IO ()
test n _ [] = print (show n ++ " tests passed")
test n p (f:fs) = 
  if p f 
  then do print ("pass on:" ++ show f)
          test n p fs
  else error ("failed test on:" ++ show f)

-- 
testForms :: Int -> (Form -> Bool) -> IO ()
testForms n p = do 
  fs <- getRandomFs n
  test n p fs

testParser = testForms 100
   (\ f -> let [g] = parse (show f) in 
           show f == show g)

arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)
