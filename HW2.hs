module HW2 where
import Data.Maybe (isJust, fromJust)
-- Group: HW2 87
-- Group members:
--  * Samuel Somatis, id#933339006, ONID somatiss@oregonstate.edu
--  * Name, ID
--  * Name, ID
-- Worked with Alexander Nead-Work 933190259 and recieved help from other classmate 'Megan' on pathTo function
-- Grading note: 10pts total
--  * 2pts each for encodeList and mapTree
--  * 3pts each for valueAt and pathTo

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Encode a list as a tree with only right branches.
--
--   >>> encodeList []
--   End
--
--   >>> encodeList [1,2,3,4]
--   Node 1 End (Node 2 End (Node 3 End (Node 4 End End)))
--
--   >>> encodeList ":-D"
--   Node ':' End (Node '-' End (Node 'D' End End))
--
encodeList :: [a] -> Tree a
-- empty
encodeList [] = End
-- just one node
encodeList [a] = Node a End End
-- else
encodeList (a:t) = Node a End (encodeList t)



-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
--maps some function onto tree a and returns tree b
mapTree :: (a -> b) -> Tree a -> Tree b
--just end
mapTree f End = End
--apply function to root a, then recursivly apply the function to the left and right subtrees
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)

-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
--Have to use Maybe, so values are Just a or Nothing
valueAt :: Path -> Tree a -> Maybe a
--empty list after path is complete or if path is initially empty (root)
valueAt [] (Node a _ _) = Just a
--when an end is reached after path
valueAt [] End = Nothing
--recurse left if the input is L:ls, because that means the current index value is L
valueAt (L:ls) (Node a b c) = valueAt ls b
valueAt (R:ls) (Node a b c) = valueAt ls c
--if the path is too long for the tree
valueAt (L:ls) End = Nothing
valueAt (R:ls) End = Nothing



-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
pathTo :: Eq a => a -> Tree a -> Maybe Path

--Nothing if an End is encountered
pathTo x End = Nothing
--Check if first value is = what we are searched for, if not, recurse on the left. If a node is reached, append L to path, if nothing is reached, do the same technique, but recurse on the right
pathTo x (Node a l r) = if(x == a) then Just [] 
                          else (case pathTo x l of
                                            Just p -> Just (L:p) 
                                            Nothing -> case pathTo x r of
                                                                    Just p -> Just (R:p)
                                                                    Nothing -> Nothing)
