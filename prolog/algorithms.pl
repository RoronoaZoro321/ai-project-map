% algorithms.pl
%
% This file contains the implementation of the shortest path algorithms: Dijkstra and A*.
% It uses a priority queue (heap) for efficiency and maintains a closed set to prevent revisiting nodes.

% Include necessary libraries
:- use_module(library(heaps)).

% Dynamic Declarations
:- dynamic edge/3.
:- dynamic node/3.
:- dynamic delayed_edge/2.

% Shortest Path Finder

% shortest_path(+StartNode, +EndNode, +Algorithm, -Path, -TotalDistance).
% Finds the shortest path between StartNode and EndNode using the specified algorithm.

shortest_path(Start, Goal, Algorithm, Path, Distance) :-
    ( Algorithm = dijkstra ->
        dijkstra(Start, Goal, Path, Distance)
    ; Algorithm = astar ->
        a_star(Start, Goal, Path, Distance)
    ).

% Dijkstras Algorithm Implementation Using a Priority Queue

dijkstra(Start, Goal, Path, Distance) :-
    G0 is 0,
    empty_heap(Open),
    add_to_heap(Open, G0, [Start, [Start], G0], Open1),
    dijkstra_loop(Open1, Goal, [], Path, Distance).

% dijkstra_loop(+OpenHeap, +GoalNode, +ClosedSet, -Path, -Distance)
dijkstra_loop(Open, Goal, Closed, Path, Distance) :-
    get_from_heap(Open, G, [Node, CurrentPath, G], RestOpen),
    ( Node == Goal ->
        reverse(CurrentPath, Path),
        Distance = G
    ; memberchk(Node, Closed) ->
        dijkstra_loop(RestOpen, Goal, Closed, Path, Distance)
    ; findall(
          [NewG, [NextNode, [NextNode | CurrentPath], NewG]],
          (
              edge(Node, NextNode, Weight),
              \+ memberchk(NextNode, Closed),
              \+ delayed_edge(Node, NextNode),
              NewG is G + Weight
          ),
          NextNodes
      ),
      foldl(add_to_heap_wrapper, NextNodes, RestOpen, NewOpen),
      dijkstra_loop(NewOpen, Goal, [Node | Closed], Path, Distance)
    ).

% A* Algorithm Implementation Using a Priority Queue

a_star(Start, Goal, Path, Distance) :-
    heuristic(Start, Goal, H0),
    G0 is 0,
    F0 is G0 + H0,
    empty_heap(Open),
    add_to_heap(Open, F0, [Start, [Start], G0], Open1),
    a_star_loop(Open1, Goal, [], Path, Distance).

% a_star_loop(+OpenHeap, +GoalNode, +ClosedSet, -Path, -Distance)
a_star_loop(Open, Goal, Closed, Path, Distance) :-
    get_from_heap(Open, _F, [Node, CurrentPath, G], RestOpen),
    ( Node == Goal ->
        reverse(CurrentPath, Path),
        Distance = G
    ; memberchk(Node, Closed) ->
        a_star_loop(RestOpen, Goal, Closed, Path, Distance)
    ; findall(
          [F, [NextNode, [NextNode | CurrentPath], NewG]],
          (
              edge(Node, NextNode, Weight),
              \+ memberchk(NextNode, Closed),
              \+ delayed_edge(Node, NextNode),
              NewG is G + Weight,
              heuristic(NextNode, Goal, H),
              F is NewG + H
          ),
          NextNodes
      ),
      foldl(add_to_heap_wrapper, NextNodes, RestOpen, NewOpen),
      a_star_loop(NewOpen, Goal, [Node | Closed], Path, Distance)
    ).

% Helper predicate to add elements to the heap
add_to_heap_wrapper([Key, Value], Heap, NewHeap) :-
    add_to_heap(Heap, Key, Value, NewHeap).
