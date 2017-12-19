newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
newtype PSet3 a = PSet3{ contains3 :: (a -> Bool) }

--Monoid
-- Вариант 1: mappend реализуется как объединение множеств
instance Monoid (PSet a) where
  mempty = PSet (\el -> False)
  mappend (PSet a) (PSet b) = PSet (\el -> (a el) || (b el))

-- Вариант 2: mappend реализуется как пересечение множеств
instance Monoid (PSet2 a) where
  mempty = PSet2 (\el -> False)
  mappend (PSet2 a) (PSet2 b) = PSet2 (\el -> (a el) && (b el))

-- Вариант 3: mappend реализуется как разность множеств (A\B)
instance Monoid (PSet3 a) where
  mempty = PSet3 (\a -> False)
  mappend (PSet3 a) (PSet3 b) = PSet3 (\el -> (a el) && (not $ b el))
-- Можно реализовать симметричную разность множеств как (A && not B) || (not A && B)

-- Functor
instance Functor PSet where
  fmap _ (PSet a) = PSet (\b -> False)

