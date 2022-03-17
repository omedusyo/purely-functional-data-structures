module BinomialHeap exposing (..)


type
    BinomialTree a
    -- * We track the rank of the tree
    -- * The subtrees are maintained in decreasing rank
    = Node a Int (List (BinomialTree a))


rank : BinomialTree a -> Int
rank (Node _ r _) =
    r


rankZero : a -> BinomialTree a
rankZero x =
    Node x 0 []


rankOne a b =
    Node a 1 [ rankZero b ]


rankTwo a b c d =
    Node a 2 [ rankOne b c, rankZero d ]


rankThree a b c d e f g h =
    Node a 3 [ rankTwo b c d e, rankOne f g, rankZero h ]



-- Assume that `t, s` have the same rank


link : BinomialTree comparable -> BinomialTree comparable -> BinomialTree comparable
link ((Node x r t_children) as t) ((Node y _ s_children) as s) =
    if x <= y then
        Node x (r + 1) (s :: t_children)

    else
        Node y (r + 1) (t :: s_children)


type alias
    BinomialHeap a
    -- * binomial trees are stored by from smallest to highest rank
    -- * binomial trees have to have unique rank in the list
    =
    List (BinomialTree a)


empty : BinomialHeap a
empty =
    []


insertTree : BinomialTree comparable -> BinomialHeap comparable -> BinomialHeap comparable
insertTree s trees0 =
    case trees0 of
        [] ->
            [ s ]

        t :: trees1 ->
            if rank s < rank t then
                s :: trees0

            else
                -- the ranks are equal
                insertTree (link s t) trees1


insert : comparable -> BinomialHeap comparable -> BinomialHeap comparable
insert y trees =
    insertTree (Node y 0 []) trees


merge : BinomialHeap comparable -> BinomialHeap comparable -> BinomialHeap comparable
merge t_trees0 s_trees0 =
    case ( t_trees0, s_trees0 ) of
        ( [], _ ) ->
            s_trees0

        ( _, [] ) ->
            t_trees0

        ( t :: t_trees1, s :: s_trees1 ) ->
            if rank t < rank s then
                t :: merge t_trees1 s_trees0

            else if rank t > rank s then
                s :: merge t_trees1 s_trees0

            else
                merge s_trees1 s_trees1 |> insertTree (link s t)


findMin : BinomialHeap comparable -> Maybe comparable
findMin trees0 =
    -- TODO: Can be made into a tail recursive function but whatever
    case trees0 of
        [] ->
            Nothing

        (Node x _ _) :: trees1 ->
            findMin trees1
                |> Maybe.map (min x)


root : BinomialTree a -> a
root (Node x _ _) =
    x


removeMinTree : BinomialHeap comparable -> Maybe ( BinomialTree comparable, BinomialHeap comparable )
removeMinTree trees0 =
    case trees0 of
        [] ->
            Nothing

        tree :: trees1 ->
            removeMinTree trees1
                |> Maybe.map
                    (\( minTree1, trees1new ) ->
                        if root tree < root minTree1 then
                            ( tree, trees1 )

                        else
                            ( minTree1, tree :: trees1new )
                    )


deleteMin : BinomialHeap comparable -> Maybe ( comparable, BinomialHeap comparable )
deleteMin trees0 =
    removeMinTree trees0
        |> Maybe.map
            (\( Node x r children, trees1 ) ->
                ( x, merge (List.reverse children) trees1 )
            )
