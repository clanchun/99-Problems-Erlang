-module(e99).

-export([last/1,
         last_but_one/1
        ]).

%%% Part 1: Lists

%% 1.01: Find the last element of a list.
last([]) ->
    throw(none);
last([X]) ->
    X;
last([_ | T]) ->
    last(T).

%% 1.02: Find the last but one element of a list.
last_but_one([]) ->
    throw(none);
last_but_one([_]) ->
    throw(none);
last_but_one([X, _]) ->
    X;
last_but_one([_ | T]) ->
    last_but_one(T).
