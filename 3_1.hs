data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber ) | Pred (WeirdPeanoNumber )
       
--type classes
instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = a == b    
    (==) (Pred a) (Pred b) = a == b
    (==) argl argr = False

instance Ord WeirdPeanoNumber where
    (<=) Zero (Pred _) = False
    (<=) Zero arg = True
    (<=) (Succ a) (Succ b) = a <= b
    (<=) (Succ a) arg = False
    (<=) (Pred a) (Pred b) = a <= b
    (<=) (Pred a) arg = True

instance Num WeirdPeanoNumber where
    (+) arg Zero = arg
    (+) Zero arg = arg
    (+) (Succ a) (Succ b) = Succ (Succ a) + b
    (+) (Succ a) (Pred b) = a + b
    (+) (Pred a) (Pred b) = Pred (Pred a) + b
    (+) (Pred a) (Succ b) = a + b    
    
    negate Zero = Zero
    negate (Succ a) = Pred (negate a)
    negate (Pred a) = Succ (negate a)
    
    abs (Pred a) = Succ (abs a)
    abs a = a

    signum Zero = Zero
    signum (Succ a) = Succ Zero
    signum (Pred a) = Pred Zero

    fromInteger a | (a == 0) = Zero
                  | (a > 0) = Succ $ fromInteger (a - 1)
                  | (a < 0) = Pred $ fromInteger (a + 1)

    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) (Succ Zero) arg = arg
    (*) arg (Succ Zero) = arg
    (*) (Succ a) (Succ b) = a * Succ b + Succ b
    (*) argl @ (Pred a) argr @ (Pred b) = negate argl * negate argr
    (*) argl @ (Succ a) argr @ (Pred b) = negate $ argl * negate argr
    (*) argl @ (Pred a) argr @ (Succ b) = negate $ negate argl * argr

instance Real WeirdPeanoNumber where
    toRational Zero = toRational 0
    toRational (Succ a) = toRational (toRational a + 1)
    toRational (Pred a) = toRational (toRational a - 1)

instance Enum WeirdPeanoNumber where
    toEnum a | (a == 0) = Zero
             | (a > 0) = Succ $ toEnum (a - 1)
             | (a < 0) = Pred $ toEnum (a + 1)

    fromEnum Zero = 0
    fromEnum (Succ a) = fromEnum a + 1
    fromEnum (Pred a) = fromEnum a - 1

instance Integral WeirdPeanoNumber where
    toInteger Zero = toInteger 0
    toInteger (Succ a) = toInteger (toInteger a + 1)
    toInteger (Pred a) = toInteger (toInteger a - 1)

    quotRem num den = 
        let quotInt n d = quot (toInteger n) (toInteger d) in
        let remInt n d = rem (toInteger n) (toInteger d) in
        (fromInteger $ quotInt num den , fromInteger $ remInt num den)
    
instance Show WeirdPeanoNumber where
    show a = show (toInteger a) ++ "p"

--fromPeano, toPeano and id functions
fromPeano :: WeirdPeanoNumber -> Integer
fromPeano Zero = 0
fromPeano (Succ a) = (fromPeano a) + 1
fromPeano (Pred a) = (fromPeano a) - 1

toPeano :: Integer -> WeirdPeanoNumber
toPeano x | x == 0 = Zero
          | x > 0 = Succ $ toPeano $ x - 1
          | x < 0 = Pred $ toPeano $ x + 1

--some tests
test1 = show $ (toPeano 6) + fst ( quotRem (toPeano 25) (toPeano 4) )
test2 = show $ (toPeano 6) * fst ( quotRem (toPeano 25) (toPeano 4) )
test3 = show $ (toPeano 6) - fst ( quotRem (toPeano (-25)) (toPeano 4) )
test4 = show $ toInteger $ (toPeano 6) - fst ( quotRem (toPeano (-25)) (toPeano 4) )
