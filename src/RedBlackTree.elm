module RedBlackTree exposing (..)


type Color
    = Red
    | Black


type
    Tree a
    -- * Every child of a Red node is Black
    -- * Every path from root to an empty node contain the same number of black nodes
    = Empty
    | Branch Color (Tree a) a (Tree a)


color : Tree a -> Color
color tree =
    case tree of
        Empty ->
            Black

        Branch c _ _ _ ->
            c


member : comparable -> Tree comparable -> Bool
member x tree =
    case tree of
        Empty ->
            False

        Branch _ tree_left y tree_right ->
            if y < x then
                member x tree_left

            else if y == x then
                True

            else
                member x tree_right


insertHelper : comparable -> Tree comparable -> Tree comparable
insertHelper x tree =
    case tree of
        Empty ->
            -- Now this is interesting... it's Red not Black
            Branch Red Empty x Empty

        Branch c tree_left y tree_right ->
            if y < x then
                branchBalanced c (insert x tree_left) y tree_right

            else if x == y then
                tree

            else
                branchBalanced c tree_left y (insert x tree_right)


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case insertHelper x tree of
        Empty ->
            Empty

        Branch _ tree_left y tree_right ->
            -- change the color of the root node to black
            Branch Black tree_left y tree_right


branchBalanced : Color -> Tree comparable -> comparable -> Tree comparable -> Tree comparable
branchBalanced col left_tree el right_tree =
    -- This is a replacement for the `Branch` constructor
    -- It's supposed to remove chains of B-R-R and turn them into R-(B,B)
    Debug.todo ""


type Tree23 a
    = Leaf1 a
    | Leaf2 a a
    | Branch1 (Tree23 a) a (Tree23 a)
    | Branch2 (Tree23 a) a (Tree23 a) a (Tree23 a)


ex0 : Tree23 Int
ex0 =
    Branch1
        (Branch2
            (Leaf2 3 7)
            10
            (Leaf1 14)
            17
            (Leaf2 20 24)
        )
        30
        (Branch1
            (Leaf1 32)
            38
            (Leaf2 42 48)
        )
