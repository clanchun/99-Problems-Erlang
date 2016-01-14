-module(e99_lc).

-import(e99_list, [kth/2,
                   reverse2/1,
                   len/1
                  ]).


-export([table/1,
         table2/1,
         table3/2,
         gray/1, gray2/1
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
        true ->
            true;
        false ->
            false;
        [X, or1 | T] ->
            or1(eval2([A, B], X), eval2([A, B], T));
        [X, and1, not1 | T] ->
            and1(eval2([A, B], X), eval2([A, B], [not1 | T]));
        [X, and1, Y | T] ->
            eval2([A, B], [and1(eval2([A, B], X), eval2([A, B], Y)) | T]);
        [not1, X | T] ->
            eval2([A, B], [not1(eval2([A, B], X)) | T]);
        [T] ->
            eval2([A, B], T);
        _ ->
            throw(illegal_expression)
    end.

%% 3.03 Truth tables for logical expressions (3).
%% L: [a, b, c ...], in E number means variable: 1 -> a, 2 -> b, 3 -> c, etc.
%% [1, or1, 2, and1, not1, 1]
table3(L, E) ->
    Vs = vars(L),
    [V ++ [eval3(V, E)] || V <- Vs].

vars([]) ->
    [];
vars([_]) ->
    [[true], [false]];
vars([_ | T]) ->
    [[A | B] || A <- [true, false], B <- vars(T)].

%% [a, and1, b, or1, c]
eval3(Vs, E) ->
    case E of
        N when is_integer(N) ->
            e99_list:kth(Vs, N);
        true ->
            true;
        false ->
            false;
        [X, or1 | T] ->
            or1(eval3(Vs, X), eval3(Vs, T));
        [X, and1, not1 | T] ->
            and1(eval3(Vs, X), eval3(Vs, [not1 | T]));
        [X, and1, Y | T] ->
            eval3(Vs, [and1(eval3(Vs, X), eval3(Vs, Y)) | T]);
        [not1, X | T] ->
            eval3(Vs, [not1(eval3(Vs, X)) | T]);
        [T] ->
            eval3(Vs, T);
        O ->
            throw({illegal_expression, O})
    end.

%% 3.04 Gray code.
gray(1) ->
    ["0", "1"];
gray(N) ->
    C = gray(N - 1),
    ["0" ++ X || X <- C] ++ ["1" ++ X || X <- e99_list:reverse2(C)].

gray2(1) ->
    ["0", "1"];
gray2(N) ->
    Cp = 
        case get({gray, N - 1}) of
            undefined ->
                C = gray2(N - 1),
                put({gray, N - 1}, C),
                C;
            Code ->
                Code
        end,
    Cn = ["0" ++ X || X <- Cp] ++ ["1" ++ X || X <- e99_list:reverse2(Cp)],
    put({gray, N}, Cn),
    Cn.

