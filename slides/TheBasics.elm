module TheBasics where
import List

-- A function

add a b = a + b

-- Point-free

sub = (-)

-- Type Annotation

glue : String -> String -> String
glue = (++)

-- Recursion and case-expression with pattern matching

fac : Int -> Int
fac n =
  case n of
    0 -> 1
    n -> n * fac (n-1)

-- Type aliases

type alias User = (Int, String)

handle : User -> String
handle (id, username) = (++) "@" username

-- Algebraic data types

type Tree a = EmptyNode | Node a (Tree a) (Tree a)

singleton v = Node v EmptyNode EmptyNode

insert : comparable -> Tree comparable -> Tree comparable
insert v tree =
  case tree of
    EmptyNode -> singleton v
    Node x left right ->
      if | v < x -> Node x (insert v left) right
         | v > x -> Node x left (insert v right)
         | otherwise -> tree
