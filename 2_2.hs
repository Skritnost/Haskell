-- foldl: функция берёт аккумулятор и первый элемент списка, возвращает аккумулятор
-- foldr: функция берёт первый с конца элемент списка и аккумулятор, возвращает аккумулятор 

-- folds
my_foldr :: (a -> b -> b) -> b -> [a] -> b
my_foldr _ acc []       = acc
my_foldr f acc (x : xs) = f x (my_foldr f acc xs)

my_foldl :: (b -> a -> b) -> b -> [a] -> b
my_foldl _ acc [] = acc
my_foldl f acc (x : xs) = my_foldl f (f acc x) xs

-- list functions
-- map
my_map :: (a -> b) -> [a] -> [b]
my_map f lst = my_foldr (\ x acc -> (f x) : acc) [] lst

-- flatMap
my_flatMap :: (a -> [b]) -> [a] -> [b]
my_flatMap f lst = my_foldr (\ x acc -> (f x) ++ acc) [] lst

-- concat
my_concat :: [a] -> [a] -> [a]
my_concat l1 l2 = my_foldr (:) l2 l1

-- filter
my_filter :: (a -> Bool) -> [a] -> [a]
my_filter f lst = my_foldr (\ x acc -> if f x then x : acc else acc) [] lst

-- maxBy
my_maxBy :: (a -> Integer) -> [a] -> a
my_maxBy f (x : xs) = my_foldr (\ x prev -> if f x > f prev then x else prev) x xs

-- minBy
my_minBy :: (a -> Integer) -> [a] -> a
my_minBy f (x : xs) = my_foldr (\ x prev -> if f x < f prev then x else prev) x xs

-- reverse
my_reverse :: [a] -> [a]
my_reverse lst = my_foldl (\ acc x -> x : acc) [] lst

-- elementAt
my_elementAt :: Integer -> [a] -> a
my_elementAt n lst = head $ my_foldl (\ acc _ -> tail acc) lst [1..n]

-- indexOf
my_indexOf :: String -> [String] -> Integer
my_indexOf s lst = my_foldl (\ index (a, b) -> if b == s then a else index) (-1) (zip [0..] lst)
