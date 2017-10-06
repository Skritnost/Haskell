data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }    
	    | Sum1 { lSum1 :: Term, rSum1 :: Term}
	    | Product1 { lProduct1 :: Term, rProduct1 :: Term}
	    | Sub1 { lSub1 :: Term, rSub1 :: Term }
            | UnMin { unMin :: Term}
	    deriving(Show,Eq)

<+>, <->, <*> :: Term -> Term -> Term
value1 <+> value2 = Sum1 value1 value2
value1 <-> value2 = Sub1 value1 value2
value1 <*> value2 = Product1 value1 value2

replaceVar :: String -> Term -> Term
replaceVar var Variable { varName = var1}
		| var1 == var = "the names are equal, enter a different name"
		| otherwise = Variable var1
replaceVar var Sum1 { lSum1, rSum1 } = Sum1 (replaceVar var lSum1) (replaceVar var rSum1)
replaceVar var Product1 { lProduct1, rProduct1 } = Product1 (replaceVar var lProduct1) (replaceVar var rProduct1)
replaceVar var Sub1 { lSub1, rSub1 } = Sub1 (replaceVar var lSub1) (replaceVar var lSub1)
replaceVar var UnMin { unMin } = UnMin (replaceVar var unMin) 
