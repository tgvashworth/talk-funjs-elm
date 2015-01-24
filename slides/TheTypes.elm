module TheTypes where

-- Type aliases

type alias UserId = Int
type alias UserName = String
type alias User = (UserId, UserName)

handle : User -> String
handle (id, username) = "@" ++ username

-- Algebraic data types

type Tree a = EmptyNode | Node a (Tree a) (Tree a)

singleton : comparable -> Tree comparable
singleton v = Node v EmptyNode EmptyNode

insert : comparable -> Tree comparable -> Tree comparable
insert v tree =
  case tree of
    EmptyNode -> singleton v
    Node x left right ->
      if | v < x -> Node x (insert v left) right
         | v > x -> Node x left (insert v right)
         | otherwise -> tree

contains : comparable -> Tree comparable -> Bool
contains v tree =
  case tree of
    EmptyNode -> False
    Node x left right ->
      if | v == x -> True
         | v < x  -> contains v left
         | v > x  -> contains v right
