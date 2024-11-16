% algorithms.pl

% =========================
% Shortest Path Finder
% =========================

% shortest_path(StartNode, EndNode, Algorithm, Path, TotalDistance).
% Find the shortest path between StartNode and EndNode using the specified algorithm.

shortest_path(Start, Goal, Algorithm, Path, Distance) :-
    ( Algorithm = dijkstra ->
        dijkstra([[Start, [Start], 0]], Goal, RevPath, Distance),
        reverse(RevPath, Path)
    ; Algorithm = astar ->
        heuristic(Start, Goal, H),
        F is H,
        a_star([[Start, [Start], 0, F]], Goal, RevPath, Distance),
        reverse(RevPath, Path)
    ).

% =========================
% Dijkstra's Algorithm
% =========================

% Comparator predicate for predsort/3
compare_dist(<, [_, _, Dist1], [_, _, Dist2]) :- Dist1 < Dist2.
compare_dist(=, [_, _, Dist1], [_, _, Dist2]) :- Dist1 =:= Dist2.
compare_dist(>, [_, _, Dist1], [_, _, Dist2]) :- Dist1 > Dist2.

% Dijkstra's helper predicate using predsort/3
dijkstra([[Goal, Path, Dist] | _], Goal, Path, Dist).
dijkstra([[CurrentNode, CurrentPath, CurrentDist] | Rest], Goal, Path, Dist) :-
    findall(
        [NextNode, [NextNode | CurrentPath], NewDist],
        (
            edge(CurrentNode, NextNode, D, Delay),
            \+ member(NextNode, CurrentPath),
            TotalEdgeDistance is D + Delay,
            NewDist is CurrentDist + TotalEdgeDistance
        ),
        NextNodes
    ),
    append(Rest, NextNodes, UpdatedQueue),
    predsort(compare_dist, UpdatedQueue, SortedQueue),
    dijkstra(SortedQueue, Goal, Path, Dist).

% =========================
% A* Algorithm
% =========================

% Comparator predicate for predsort/3
compare_f(<, [_, _, _, F1], [_, _, _, F2]) :- F1 < F2.
compare_f(=, [_, _, _, F1], [_, _, _, F2]) :- F1 =:= F2.
compare_f(>, [_, _, _, F1], [_, _, _, F2]) :- F1 > F2.

% a_star(OpenList, GoalNode, Path, TotalCost).
% Implements the A* search algorithm.

a_star([[Goal, Path, G, _] | _], Goal, Path, G).
a_star([[CurrentNode, CurrentPath, CurrentG, _] | Rest], Goal, Path, Distance) :-
    findall(
        [NextNode, [NextNode | CurrentPath], NewG, NewF],
        (
            edge(CurrentNode, NextNode, D, Delay),
            \+ member(NextNode, CurrentPath),
            TotalEdgeDistance is D + Delay,
            NewG is CurrentG + TotalEdgeDistance,
            heuristic(NextNode, Goal, H),
            NewF is NewG + H
        ),
        NextNodes
    ),
    append(Rest, NextNodes, UpdatedQueue),
    predsort(compare_f, UpdatedQueue, SortedQueue),
    a_star(SortedQueue, Goal, Path, Distance).
