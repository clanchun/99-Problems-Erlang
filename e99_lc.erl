-module(e99_lc).

-export([table/1,
         table2/1
        ]).

%% 3.01 Truth tables for logical expressions.
table(E) ->
    Vs = [[A, B] || A <- [true, false], B <- [true, false]],
    [[A, B, eval([A, B], E)] || [A, B] <- Vs].

eval([A, B], E) ->
    case E of
        a ->
            A;
        b ->
            B;
        [and1, X, Y] ->
            and1(eval([A, B], X), eval([A, B], Y));
        [or1, X, Y] ->
            or1(eval([A, B], X), eval([A, B], Y));
        [nand, X, Y] ->
            nand(eval([A, B], X), eval([A, B], Y));
        [nor, X, Y] ->
            nor(eval([A, B], X), eval([A, B], Y));
        [xor1, X, Y] ->
            xor1(eval([A, B], X), eval([A, B], Y));
        [impl, X, Y] ->
            impl(eval([A, B], X), eval([A, B], Y));
        [equ, X, Y] ->
            equ(eval([A, B], X), eval([A, B], Y));
        _ ->
            throw(illegal_expression)
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

not1(true) ->
    false;
not1(false) ->
    true.

%% 3.02 Truth tables for logical expressions (2).
table2(E) ->
    Vs = [[A, B] || A <- [true, false], B <- [true, false]],
    [[A, B, eval2([A, B], E)] || [A, B] <- Vs].

%% operator precedence: [] > not1 > and1 > or1, [] as ()
eval2([A, B], E) ->
    case E of
        a ->
            A;
        b ->
            B;
        [X, or1 | T] ->
            or1(eval2([A, B], X), eval2([A, B], T));
        [X, and1 | T] ->
            and1(eval2([A, B], X), eval2([A, B], T));
        [not1 | T] ->
            not1(eval2([A, B], T));
        [T] ->
            eval2([A, B], T);
        _ ->
            throw(illegal_expression)
    end.
