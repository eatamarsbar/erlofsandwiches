-module(tail_recursion).

-export([fac/1, quicksort/1]).

%% A recursive function can be transformed into an iterative function using a "tail recursion" technique.

%% Factorial
fac(N) ->
    fac(N, 1).

fac(0, Acc) ->
    Acc;
fac(N, Acc) when N > 0 ->
    fac(N - 1, N * Acc).

%% Quick-Sort
quicksort([]) ->
    [];
quicksort([Pivot | Rest]) ->
    {Smaller, Larger} = quicksort_partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

quicksort_partition(_, [], Smaller, Larger) ->
    {Smaller, Larger};
quicksort_partition(Pivot, [H | T], Smaller, Larger) ->
    if
        H =< Pivot ->
            quicksort_partition(Pivot, T, [H | Smaller], Larger);
        true ->
            quicksort_partition(Pivot, T, Smaller, [H | Larger])
    end.

%% input: [2,2,1,3,3]
%% output: [1,2,2,3,3]
%%
%% partition(2,[2|1,3,3],[],[])
%% S = [2], L = []
%% partition(2,[1|3,3],[2],[])
%% S = [1,2], L = []
%% partition(2,[3|3],[2],[1])
%% S = [1,2], L = [3]
%% partition(2,[3],[3,2],[1])
%% S = [1,2], L = [3,3]
%% partition end
