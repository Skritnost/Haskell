data FunMonad a = FunMonad { fun :: () -> a } 

-- Монада должна быть функтором
instance Functor FunMonad where
    fmap f (FunMonad a) = FunMonad $ \() -> f (a ()) 

-- Для монады обязательно реализуются функции bind, return и fail
instance Monad FunMonad where
    a >>= f =  f $ fun a () 
    return a  = FunMonad $ \() -> a
    fail = error

-- Для монады должен быть инстанс класса типов Applicative
instance Applicative FunMonad where
    pure x = FunMonad $ \() -> x
    FunMonad a <*> FunMonad b = FunMonad $ \() -> (a ()) (b ())

-- Чтобы понимать, когда FunMonad, а когда просто число
instance (Show a) => Show (FunMonad a) where
    show (FunMonad a) = "FunMonad " ++ (show $ a ())


fm =  FunMonad $ \()-> 7     -- FunMonad 7
f x = FunMonad $ \() -> 3+x  -- FunMonad 3+

test_fmap = fmap toRational fm    -- FunMonad 7 % 1
test_bind = fm >>= f              -- FunMonad 10
