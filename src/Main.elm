module Main exposing (..)

import BinomialHeap
import LeftistHeap
import LeftistHeapSizeRank


type BinTree
    = Nil
    | T BinTree BinTree



-- This is inexpensive


flipChildren : BinTree -> BinTree
flipChildren t =
    case t of
        Nil ->
            Nil

        T t0 t1 ->
            T t1 t0



-- This should create a complete binary tree of height `h`
-- and the space should be proportional to `h`, not `2**h`


complete : Int -> BinTree
complete h =
    if h == 0 then
        Nil

    else
        let
            t =
                complete (h - 1)
        in
        T t t


type ApplyResult a
    = Finished a
    | Continue (List a)



-- [a, b, c, d, e, f, g, h]
-- [op(a, b), op(c, d), op(e, f), op(g, h)]
-- [op(op(a, b), op(c, d)), op(op(e, f), op(g, h))]
--  op(op(op(a, b), op(c, d)), op(op(e, f), op(g, h)))
--
-- [a, b, c]
-- op(op(a, b), c)


apply2 : (a -> a -> a) -> a -> List a -> ApplyResult a
apply2 op neutral xs =
    case xs of
        [] ->
            Finished neutral

        [ x ] ->
            Finished x

        x :: y :: xs_ ->
            case apply2 op neutral xs of
                Finished z ->
                    Finished (op (op x y) z)

                Continue ys ->
                    Continue (op x y :: ys)
