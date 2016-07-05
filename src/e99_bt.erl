-module(e99_bt).

-export([istree/1,
         cbal_tree/1
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
