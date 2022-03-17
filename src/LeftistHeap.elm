module LeftistHeap exposing (..)

-- Assuming `a` is equipped with <= ordering relation,
-- we maintain an invariant:
--  (*) Given `Branch r a t0 t1 : Heap a`
--      then for all b element of t0 or t1, a <= b
--  (*) Given `Branch r a t0 t1 : Heap a`,
--      then rank(t0) >= rank(t1)


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



-- assume `t <= x <= s` where by `t <= x` we mean for each element y of `t`, `y <= t` etc


makeT : Heap a -> a -> Heap a -> Heap a
makeT t x s =
    if rank t >= rank s then
        Branch (rank s + 1) t x s

    else
        Branch (rank t + 1) s x t


merge : Heap comparable -> Heap comparable -> Heap comparable
merge t s =
    case ( t, s ) of
        ( Empty, _ ) ->
            s

        ( _, Empty ) ->
            t

        ( Branch t_rank t0 x t1, Branch s_rank s0 y s1 ) ->
            if x <= y then
                makeT t0 x (merge t1 s)

            else
                makeT s0 y (merge t s1)


insert : Heap comparable -> comparable -> Heap comparable
insert t y =
    case t of
        Empty ->
            singleton y

        Branch t_rank t0 x t1 ->
            if x <= y then
                makeT t0 x (insert t1 y)

            else
                makeT t0 y (insert t1 x)


insert0 : Heap comparable -> comparable -> Heap comparable
insert0 t y =
    merge t (singleton y)


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



-- Trying out a different representation


type MyHeap a
    = Spine ( Int, List ( a, MyHeap a ) )


emptyMyHeap =
    Spine ( 0, [] )


singletonMyHeap a =
    Spine ( 1, [ ( a, emptyMyHeap ) ] )


spine : List ( a, MyHeap a ) -> MyHeap a
spine xs =
    Spine ( List.length xs, xs )


example0 : MyHeap Int
example0 =
    spine
        [ ( 10
          , spine
                [ ( 20, spine [ ( 22, spine [] ) ] )
                , ( 25, spine [ ( 45, spine [] ) ] )
                ]
          )
        , ( 30
          , spine
                [ ( 50
                  , spine
                        [ ( 60, spine [] )
                        ]
                  )
                , ( 70, spine [] )
                ]
          )
        , ( 40, spine [] )
        ]


f : (a -> a -> a) -> a -> List a -> a
f op neutral xs =
    case xs of
        [] ->
            neutral

        _ ->
            Debug.todo ""



-- ===Using a different definition of rank (in terms of size, not the length of the rightmost path)
