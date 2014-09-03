module Sol1 where
import GS

-- Exercise 1.1
calculations x = (x + x) * x - (x / x)

-- Exercise 1.2 & 1.3
-- Can not find a actual question..

-- Exercise 1.4
countToN n = countoToNsub 0 n []
    where
        counToSub 0 n xs = xs
