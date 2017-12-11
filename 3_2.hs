data ReverseList a = RNil | RCons (ReverseList a) a

--fromList and toList
toList:: ReverseList a -> [a]
toList revList = 
    let makeList RNil = []
        makeList (RCons start end) = end : makeList start in
    reverse $ makeList revList 

fromList:: [a] -> ReverseList a
fromList list = foldl (\ revList elem -> RCons revList elem) RNil list

--type classes
instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show (RCons RNil end) = show end
    show (RCons start end) = show start ++ "," ++ show end

instance (Eq a) => Eq (ReverseList a) where
    (==) argl argr = toList argl == toList argr

instance (Ord a) => Ord (ReverseList a) where
    (<=) argl argr = toList argl <= toList argr

instance Monoid (ReverseList a) where
    mempty = RNil    
    mappend RNil arg = arg
    mappend arg RNil = arg
    mappend argl argr =
        foldl (\ revList elem -> RCons revList elem) argl (toList argr)

instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap arg (RCons start end) = RCons (fmap arg start) (arg end)
