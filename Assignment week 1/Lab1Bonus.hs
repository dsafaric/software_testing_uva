module Lab1Bonus

where 

data Creature = Lady | Tiger 
     deriving (Eq,Show)

sign1, sign2 :: (Creature,Creature) -> Bool
sign1 (x,y) = x == Lady && y == Tiger
sign2 (x,y) = x /= y

solution1 :: [(Creature,Creature)]
solution1 = 
 [ (x,y) | x <- [Lady,Tiger], 
           y <- [Lady,Tiger], 
           sign1 (x,y) /= sign2 (x,y) ]

data Islander = Knight | Knave deriving (Eq,Show)

john :: (Islander,Islander) -> Bool
john (x,y) = (x,y) == (Knave,Knave)

solution3 :: [(Islander,Islander)]
solution3 = [(x,y) | x <- [Knight,Knave], 
                     y <- [Knight,Knave], 
                     john (x,y) == (x == Knight) ]

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

matthew, peter, jack, arnold, carl :: Boy -> Bool
matthew = \ x -> not (x==Matthew) && not (x==Carl)
peter   = \ x -> x==Matthew || x==Jack
jack    = \ x -> not (matthew x) && not (peter x)
arnold  = \ x -> matthew x /= peter x
carl    = \ x -> not (arnold x)

declarations = [matthew,peter,jack,arnold,carl]
table = zip declarations boys 
