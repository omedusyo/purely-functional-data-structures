module LeftistHeapSizeRank exposing (..)


type Heap a
    = Empty
    | Branch Int (Heap a) a (Heap a)


show : (a -> String) -> Heap a -> String
show toString t =
    case t of
        Empty ->
            ""

        Branch _ t0 x t1 ->
            case ( t0, t1 ) of
                ( Empty, Empty ) ->
                    toString x

                ( Branch _ _ _ _, Empty ) ->
                    -- (...) x _
                    String.concat [ "(", show toString t0, ") ", toString x, " _" ]

                ( Empty, Branch _ _ _ _ ) ->
                    -- _ x (...)
                    String.concat [ "_ ", toString x, "(", show toString t1, ")" ]

                ( Branch _ _ _ _, Branch _ _ _ _ ) ->
                    -- (...) x (...)
                    String.concat [ "(", show toString t0, ") ", toString x, " (", show toString t1, ")" ]


singleton : a -> Heap a
singleton x =
    Branch 1 Empty x Empty


rank : Heap a -> Int
rank t =
    case t of
        Empty ->
            0

        Branch r _ _ _ ->
            r


minMax : comparable -> comparable -> ( comparable, comparable )
minMax x y =
    if x <= y then
        ( x, y )

    else
        ( y, x )


insert : Heap comparable -> comparable -> Heap comparable
insert t y =
    -- Note that here we don't do any flips
    case t of
        Empty ->
            singleton y

        Branch t_rank t0 x t1 ->
            let
                ( min_, max_ ) =
                    minMax x y
            in
            if rank t0 > rank t1 then
                Branch (t_rank + 1) t0 min_ (insert t1 max_)

            else
                -- here we can assume that `rank t0 == rank t1` (this follows from the previous case and the invariant)
                Branch (t_rank + 1) (insert t0 max_) min_ t1


merge : Heap comparable -> Heap comparable -> Heap comparable
merge t s =
    case ( t, s ) of
        ( Empty, _ ) ->
            s

        ( _, Empty ) ->
            t

        ( Branch t_size t0 x t1, Branch s_size s0 y s1 ) ->
            if x <= y then
                if rank t1 + rank s >= rank t0 then
                    Branch (t_size + s_size) (merge t1 s) x t0

                else
                    Branch (t_size + s_size) t0 x (merge t1 s)

            else
            --
            if
                rank s1 + rank t >= rank s0
            then
                Branch (t_size + s_size) (merge s1 t) y s0

            else
                Branch (t_size + s_size) s0 y (merge s1 t)


pop : Heap comparable -> Maybe ( comparable, Heap comparable )
pop t =
    case t of
        Empty ->
            Nothing

        Branch _ t0 x t1 ->
            Just ( x, merge t0 t1 )


min : Heap a -> Maybe a
min t =
    case t of
        Empty ->
            Nothing

        Branch _ _ x _ ->
            Just x


listToHeap : List comparable -> Heap comparable
listToHeap xs =
    -- TODO: Can this be implemented more efficiently?
    List.foldl (\x heap -> insert heap x) Empty xs


heapToList : Heap comparable -> List comparable
heapToList t0 =
    case pop t0 of
        Nothing ->
            []

        Just ( x, t1 ) ->
            x :: heapToList t1


sort : List comparable -> List comparable
sort =
    listToHeap >> heapToList
