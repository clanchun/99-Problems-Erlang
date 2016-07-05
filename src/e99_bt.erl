-module(e99_bt).

-export([istree/1,
         cbal_tree/1,
         symmetric/1
        ]).



%% {value, left, right} | nil

%% 4.01 Check whether a given term represents a binary tree
istree(nil) ->
    true;
istree({_, Left, Right}) ->
    istree(Left) andalso istree(Right);
istree(_) ->
    false.

%% 4.02 Construct completely balanced binary trees
cbal_tree(0) ->
    [nil];
cbal_tree(N) ->
    M = N - 1,
    L = M div 2,
    R = M - L,
    Left = cbal_tree(L),
    Right = cbal_tree(R),
    
    if
        L == R ->
            [{x, LT, LR} || LT <- Left, LR <- Right];
        true ->
            [{x, LT, LR} || LT <- Left, LR <- Right] ++ 
                [{x, LT, LR} || LT <- Right, LR <- Left]
    end.

%% 4.03 Symmetric binary trees
symmetric(T) ->
    mirror(T, T).

mirror(nil, nil) ->
    true;
mirror(nil, _) ->
    false;
mirror(_, nil) ->
    false;
mirror({_, nil, nil}, {_, nil, nil}) ->
    true;
mirror({_, nil, nil}, {_, _, nil}) ->
    false;
mirror({_, nil, nil}, {_, nil, _}) ->
    false;
mirror({_, _, nil}, {_, nil, nil}) ->
    false;
mirror({_, nil, _}, {_, nil, nil}) ->
    false;
mirror({_, La, Ra}, {_, Lb, Rb}) ->
    mirror(La, Rb) andalso mirror(Lb, Ra).
