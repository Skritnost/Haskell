data BinaryTree = EmptyTree
                | Leaf Integer
                | Node Integer BinaryTree BinaryTree deriving Show

-- add element
insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = Leaf x
insert (Leaf l) x | x < l    = Node l (Leaf x) EmptyTree
                  | x > l = Node l EmptyTree (Leaf x)
                  | otherwise = Leaf l                   
insert (Node v l r) x | x < v = Node v (insert l x) r
                      | x > v = Node v l (insert r x)
                      | otherwise = Node v l r

-- delete element
remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree
remove (Leaf l) x = if (l == x) then EmptyTree else Leaf l
remove (Node v l r) x | x < v = Node v (remove l x) r
                      | x > v = Node v l (remove r x)

-- If v = x, delete square nod, last two poddereva combine in new nod, choose like a square - square left poddereva
                      | otherwise = combine l r
                      where combine EmptyTree t = t
                            combine t EmptyTree = t
                            combine (Leaf l) t = Node l EmptyTree t
                            combine (Node v l r) t = Node v l (combine r t)

-- Create empty tree
emptyTree :: BinaryTree
emptyTree = EmptyTree

-- search element in tree
containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf l)  x = x == l
containsElement (Node v l r) x | x < v = containsElement l x
                               | x > v = containsElement r x
                               | otherwise = v == x

-- Search in tree least element, which is bigger or equal to this
nearestE EmptyTree _ = error "Not found!"
nearestGE (Leaf a) x = if (x < a) then a else error "Not found!"
nearestGE (Node v l r) x | v == x = v
                         | v < x  = nearestGE r x
                         | v > x  = nearestGE l x

-- Create tree from list
treeFromList :: [Integer] -> BinaryTree
treeFromList lst = foldl insert EmptyTree lst

-- Create list from tree
listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []
listFromTree (Leaf l) = [l]
listFromTree (Node v l r) = (listFromTree l) ++ [v] ++ (listFromTree r)
