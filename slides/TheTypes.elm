module TheTypes where

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

element : comparable -> Tree comparable -> Bool
element v tree =
  case tree of
    EmptyNode -> False
    Node x left right ->
      if | v == x -> True
         | v < x  -> element v left
         | v > x  -> element v right
