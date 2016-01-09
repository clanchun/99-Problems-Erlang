-module(e99_lc).

-export([table/1]).

%% 3.01 Truth tables for logical expressions.
table(E) ->
    [eval(A, B, E) || A <- [true, false], B <- [true, false]].

eval(A, B, E) ->
    case E of
        a ->
            A;
        b ->
            B;
        [and1, X, Y] ->
            and1(eval(A, B, X), eval(A, B, Y));
        [or1, X, Y] ->
            or1(eval(A, B, X), eval(A, B, Y));
        [nand, X, Y] ->
            nand(eval(A, B, X), eval(A, B, Y));
        [nor, X, Y] ->
            nor(eval(A, B, X), eval(A, B, Y));
        [xor1, X, Y] ->
            xor1(eval(A, B, X), eval(A, B, Y));
        [impl, X, Y] ->
            impl(eval(A, B, X), eval(A, B, Y));
        [equ, X, Y] ->
            equ(eval(A, B, X), eval(A, B, Y));
        _ ->
            illegal_expression
    end.

and1(true, true) ->
    true;
and1(_, _) ->
    false.

or1(false, false) ->
    false;
or1(_, _) ->
    true.

nand(A, B) ->
    not and1(A, B).

nor(A, B) ->
    not or1(A, B).

xor1(A, A) ->
    false;
xor1(_, _) ->
    true.

impl(A, B) ->
    or1(not A, B).

equ(A, A) ->
    true;
equ(_, _) ->
    false.
