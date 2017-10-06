data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }    
            | BinaryTerm{ lhv :: Term, rhv :: Term }
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

