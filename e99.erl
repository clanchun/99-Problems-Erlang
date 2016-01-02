-module(e99).

-export([last/1,
         last_but_one/1,
         kth/2,
         len/1,
         reverse/1, reverse2/1,
         is_palindrome/1,
         flatten/1,
         compress/1,
         pack/1,
         length_encode/1,
         length_encode2/1,
         length_decode/1,
         length_encode3/1,
         duplicate/1,
         replicate/2,
         drop/2,
         split/2,
         slice/3,
         rotate/2,
         remove/2,
         insert/3,
         range/2,
         random_select/2,
         random_select2/2,
         random_permu/1,
         combination/2,
         group/2
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

%% 1.03: Find the K'th element of a list.
kth([], _) ->
    throw(none);
kth([X | _], 1) ->
    X;
kth([_ | T], N) ->
    kth(T, N - 1).

%% 1.04: Find the number of elements of a list.
len([]) ->
    0;
len([_ | T]) ->
    1 + len(T).

%% 1.05: Reverse a list.
reverse([]) ->
    [];
reverse([X | T]) ->
    reverse(T) ++ [X].

reverse2(L) ->
    reverse2(L, []).

reverse2([], Acc) ->
    Acc;
reverse2([X | T], Acc) ->
    reverse2(T, [X | Acc]).

%% 1.06: Find out whether a list is a palindrome.
is_palindrome(L) ->
    is_palindrome(L, reverse2(L)).

is_palindrome([], []) ->
    true;
is_palindrome([X | T1], [X | T2]) ->
    is_palindrome(T1, T2);
is_palindrome(_, _) ->
    false.

%% 1.07: Flatten a list
flatten([]) ->
    [];
flatten([X  | T]) when not is_list(X) ->
    [X | flatten(T)];
flatten([X | T]) ->
    flatten(X) ++ flatten(T).

%% 1.08: Eliminate consecutive duplicates of list elements
compress([]) ->
    [];
compress([X]) ->
    [X];
compress([X, X | T]) ->
    compress([X | T]);
compress([X, Y | T]) ->
    [X, Y | compress(T)].

%% 1.09 Pack consecutive duplicates of list elements into sublists.
pack(L) ->
    pack(L, []).

pack([], _) ->
    [];
pack([X], [X | _] = Acc) ->
    [[X | Acc]];
pack([X], _) ->
    [[X]];
pack([X, X | T], Acc) ->
    pack([X | T], [X | Acc]);
pack([X, Y | T], Acc) ->
    [[X | Acc] | pack([Y | T], [])].

%% 1.10: Run-length encoding of a list.
length_encode(L) ->
    L1 = pack(L),
    lencode(L1).

lencode([]) ->
    [];
lencode([[X | _] = T1 | T2]) ->
    [{len(T1), X} | lencode(T2)].

%% 1.11: Modified run-length encoding.
length_encode2(L) ->
    L1 = pack(L),
    lencode2(L1).

lencode2([]) ->
    [];
lencode2([[X] | T2]) ->
    [X | lencode2(T2)];
lencode2([[X | _] = T1 | T2]) ->
    [{len(T1), X} | lencode2(T2)].

%% 1.12: Decode a run-length encoded list.
length_decode([]) ->
    [];
length_decode([{N, X} | T]) ->
    uncompress(N, X) ++ length_decode(T);
length_decode([X | T]) ->
    [X | length_decode(T)].

uncompress(0, _) ->
    [];
uncompress(N, X) ->
    [X | uncompress(N - 1, X)].

%% 13: Run-length encoding of a list (direct solution).
length_encode3(L) ->
    lencode3(L, 0).

lencode3([], _) ->
    [];
lencode3([X], 0) ->
    [X];
lencode3([X], N) ->
    [{N + 1, X}];
lencode3([X, X | T], N) ->
    lencode3([X | T], N + 1);
lencode3([X, Y | T], 0) ->
    [X | lencode3([Y | T], 0)];
lencode3([X, Y | T], N) ->
    [{N + 1, X} | lencode3([Y | T], 0)].

%% 1.14: Duplicate the elements of a list.
duplicate([]) ->
    [];
duplicate([X | T]) ->
    [X, X | duplicate(T)].

%% 1.15: Replicate the elements of a list a given number of times
replicate([], _) ->
    [];
replicate([X | T], N) ->
    uncompress(N, X) ++ replicate(T, N).

%% 1.16 Drop every N'th element from a list.
drop(T, N) ->
    drop(T, N, 1).

drop([], _, _) ->
    [];
drop([_ | T], N, N) ->
    drop(T, N, 1);
drop([X | T], N, M) ->
    [X | drop(T, N, M + 1)].
    
%% 1.17 Split a list into two parts; the length of the first part is given.
split(L, N) ->
    split(L, [], N).

split([], Acc, _) ->
    {Acc, []};
split(L, Acc, 0) ->
    {Acc, L};
split([X | T], Acc, N) ->
    split(T, Acc ++ [X], N - 1).

%% 1.18 Extract a slice from a list.
slice(L, S, E) ->
    {_, L1} = split(L, S - 1),
    {L2, _} = split(L1, E - S + 1),
    L2.

%% 1.19 Rotate a list N places to the left.
rotate(L, N) when N >= 0 ->
    {Left, Right} = split(L, N),
    Right ++ Left;
rotate(L, N) ->
    {Left, Right} = split(L, len(L) + N),
    Right ++ Left.

%% 1.20 Remove the K'th element from a list.
remove([], _) ->
    throw(none);
remove([X | T], 1) ->
    {X, T};
remove([X | T], N) ->
    {Y, L1} = remove(T, N - 1),
    {Y, [X | L1]}.

%% 1.21 Insert an element at a given position into a list.
insert(L, 0, X) ->
    [X | L];
insert([], _, _) ->
    throw(out_of_range);
insert([Y | T], N, X) ->
    [Y | insert(T, N - 1, X)].

%% 1.22 Create a list containing all integers within a given range.
range(S, S) ->
    [S];
range(S, E) ->
    [S | range(S + 1, E)].

%% 1.23 Extract a given number of randomly selected elements from a list.
random_select([], N) when N > 0 ->
    throw(too_many);
random_select(_, 0) ->
    [];
random_select(L, N) ->
    I = random:uniform(len(L)),
    {X, L1} = remove(L, I),
    [X | random_select(L1, N - 1)].

%% 1.24 Lotto: Draw N different random numbers from the set 1..M.
random_select2(M, N) ->
    random_select(range(1, M), N).

%% 1.25 Generate a random permutation of the elements of a list.
random_permu(L) ->
    random_select(L, len(L)).

%% 1.26 Generate the combinations of K distinct objects chosen from the N elements of a list
combination([], _) ->
    [];
combination([X | T], 1) ->
    [[X] | combination(T, 1)];
combination([X | T], K) ->
    Y = combination(T, K - 1),
    [[X | Z] || Z <- Y] ++ combination(T, K).

%% 1.27 Group the elements of a set into disjoint subsets. 
group([], _) ->
    [];
group(_, []) ->
    [];
group(L, [X | T]) ->
    C = combination(L, X),
    times(C, L, T, []).

times(C, _, [], G) ->
    [C | G];
times([], _, _, G) ->
    G;
times([X | T], L, S, G) ->
    Y = group(L -- X, S),
    W = [[X | Z] || Z <- Y],
    times(T, L, S, W ++ G).
         
