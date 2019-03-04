-module(astar).

-export([a_star/3, test_a_star/0]).

test_a_star() ->
    %% A/0->B/1->D/2->E/3=6
    %% A/0->C/2->E/2=4 (optimal)
    Data = [{"A", "B", 1}, {"A", "C", 2}, {"B", "D", 2}, {"C", "E", 2}, {"D", "E", 3}],
    a_star("A", "E", Data).

%% A* reference: https://en.wikipedia.org/wiki/A*_search_algorithm
a_star(Start, Goal, Data) ->
    %% Discovered nodes that have been evaluated.
    ClosedSet = sets:new(),
    %% Discovered nodes that are not yet evaluated.
    OpenSet = [{Start, 0}],
    %% A map of node->best previous node.
    BestPaths = maps:new(),
    BestPaths2 = maps:put(Start, {Start, 0}, BestPaths),
    %% Do work...
    BestPaths3 = a_star_iterate_open_set(ClosedSet, OpenSet, BestPaths2, Goal, Data),
    a_star_reconstruct_best_path(Start, Goal, BestPaths3).

%% Evaluates the open set of nodes until either the goal is found or there are
%% no more nodes to visit.
a_star_iterate_open_set(_, [], BestPaths, _, _) ->
    BestPaths;
a_star_iterate_open_set(ClosedSet, [Current | OpenSet], BestPaths, Goal, Data) ->
    io:format("===== Current: ~p, OpenSet: ~p~n", [Current, OpenSet]),
    {CurrentNode, _CurrentCost} = Current,
    case CurrentNode =:= Goal of
        %% Current node is the goal: done!
        true ->
            a_star_iterate_open_set(ClosedSet, [], BestPaths, Goal, Data);
        %% Check the neighbors for current node.
        false ->
            ClosedSet2 = sets:add_element(CurrentNode, ClosedSet),
            Neighbors = [{Start, End, Cost} || {Start, End, Cost} <- Data, Start =:= CurrentNode],
            io:format("Neighbors: ~p~n", [Neighbors]),
            {OpenSet2, BestPaths2} = a_star_iterate_node_neighbors(ClosedSet, OpenSet, BestPaths, Neighbors),
            OpenSet3 = lists:sort(fun({NodeA, CostA}, {NodeB, CostB}) -> (CostA < CostB) and (NodeA < NodeB) end, OpenSet2),
            io:format("New OpenSet: ~p~n", [OpenSet3]),
            a_star_iterate_open_set(ClosedSet2, OpenSet3, BestPaths2, Goal, Data)
    end.

%% Gets an array of nodes representing the best path from start to goal.
%% [Paths] is a map of node->best previous node.
a_star_reconstruct_best_path(Start, Goal, Paths) ->
    a_star_reconstruct_best_path([], Start, Goal, Paths).

a_star_reconstruct_best_path(BestPath, _, _, []) ->
    BestPath;
a_star_reconstruct_best_path(BestPath, Start, Goal, Paths) ->
    case Start =:= Goal of
        true ->
            a_star_reconstruct_best_path([Goal | BestPath], Start, Goal, []);
        false ->
            {PreviousNode, _} = maps:get(Goal, Paths),
            a_star_reconstruct_best_path([Goal | BestPath], Start, PreviousNode, Paths)
    end.

%% Evalutes the neighbors of a node, adding unvisited nodes to the open set
%% and updating the current best path for each neighbor.
a_star_iterate_node_neighbors(_, OpenSet, BestPaths, []) ->
    {OpenSet, BestPaths};
a_star_iterate_node_neighbors(ClosedSet, OpenSet, BestPaths, [H | T]) ->
    {From, To, Cost} = H,
    {_, FromBestScore} = maps:get(From, BestPaths),
    case sets:is_element(To, ClosedSet) of
        %% Node already visited: continue.
        true ->
            a_star_iterate_node_neighbors(ClosedSet, OpenSet, BestPaths, T);
        false ->
            %% TODO: implement g(n)
            GScore = FromBestScore + Cost,
            case lists:any(fun({Node, _}) -> Node =:= To end, OpenSet) of
                %% Node already in open set: continue.
                true ->
                    OpenSet2 = OpenSet,
                    {_, ToBestScore} = maps:get(To, BestPaths),
                    case GScore >= ToBestScore of
                        %% Score is not better: continue.
                        true ->
                            BestPaths2 = BestPaths;
                        %% Score is better: update the current best path.
                        false ->
                            BestPaths2 = maps:put(To, {From, GScore}, BestPaths) %% current best path
                    end;
                %% Add node to open set.
                false ->
                    OpenSet2 = [{To, Cost} | OpenSet],
                    BestPaths2 = maps:put(To, {From, GScore}, BestPaths) %% current best path
            end,
            %% TODO: implement h(n)
            a_star_iterate_node_neighbors(ClosedSet, OpenSet2, BestPaths2, T)
    end.
