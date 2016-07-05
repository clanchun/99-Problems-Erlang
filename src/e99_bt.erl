-module(e99_bt).

-export([istree/1]).



%% {value, left, right} | nil

%% 4.01 Check whether a given term represents a binary tree
istree(nil) ->
    true;
istree({_, Left, Right}) ->
    istree(Left) andalso istree(Right);
istree(_) ->
    false.
