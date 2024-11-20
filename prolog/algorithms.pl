/* =============================================================================
   algorithms.pl

   Description:
   -----------
   This file contains the implementation of the shortest path algorithms: Dijkstra
   and A*. It utilizes a priority queue (heap) for efficiency and maintains a closed
   set to prevent revisiting nodes. Additionally, it includes predicates to assign
   random delays to specific edges based on a given probability.
   
   =============================================================================
*/

/* =============================================================================
   Library Imports
   =============================================================================
*/
:- use_module(library(heaps)).

/* =============================================================================
   Dynamic Predicate Declarations
   =============================================================================
*/
:- dynamic edge/3.          % edge(Node1, Node2, Weight)
:- dynamic node/3.          % node(Name, Lat, Lon)
:- dynamic delayed_edge/2.  % delayed_edge(Node1, Node2)

/* =============================================================================
   Shortest Path Finder
   =============================================================================
*/

/* 
   shortest_path(+StartNode, +EndNode, +Algorithm, -Path, -TotalDistance)
   Finds the shortest path between StartNode and EndNode using the specified algorithm.
   
   Parameters:
   - StartNode: The starting node identifier.
   - EndNode: The destination node identifier.
   - Algorithm: The algorithm to use ('dijkstra' or 'astar').
   - Path: The resulting list of nodes representing the shortest path.
   - TotalDistance: The total distance of the path in the same units as edge weights.
*/
shortest_path(Start, Goal, Algorithm, Path, Distance) :-
    ( Algorithm = dijkstra ->
        dijkstra(Start, Goal, Path, Distance)
    ; Algorithm = astar ->
        a_star(Start, Goal, Path, Distance)
    ).

/* =============================================================================
   Dijkstra's Algorithm Implementation Using a Priority Queue
   =============================================================================
*/

/* 
   dijkstra(+StartNode, +GoalNode, -Path, -Distance)
   Implements Dijkstra's algorithm to find the shortest path.
   
   Parameters:
   - StartNode: The starting node identifier.
   - GoalNode: The destination node identifier.
   - Path: The resulting list of nodes representing the shortest path.
   - Distance: The total distance of the path.
*/
dijkstra(Start, Goal, Path, Distance) :-
    G0 is 0,
    empty_heap(Open),
    add_to_heap(Open, G0, [Start, [Start], G0], Open1),
    dijkstra_loop(Open1, Goal, [], Path, Distance).

/* 
   dijkstra_loop(+OpenHeap, +GoalNode, +ClosedSet, -Path, -Distance)
   The main loop for Dijkstra's algorithm.
*/
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

/* 
   add_to_heap_wrapper(+Element, +Heap, -NewHeap)
   Helper predicate to add elements to the heap.
*/
add_to_heap_wrapper([Key, Value], Heap, NewHeap) :-
    add_to_heap(Heap, Key, Value, NewHeap).

/* =============================================================================
   A* Algorithm Implementation Using a Priority Queue
   =============================================================================
*/

/* 
   a_star(+StartNode, +GoalNode, -Path, -Distance)
   Implements the A* algorithm to find the shortest path using heuristics.
   
   Parameters:
   - StartNode: The starting node identifier.
   - GoalNode: The destination node identifier.
   - Path: The resulting list of nodes representing the shortest path.
   - Distance: The total distance of the path.
*/
a_star(Start, Goal, Path, Distance) :-
    heuristic(Start, Goal, H0),
    G0 is 0,
    F0 is G0 + H0,
    empty_heap(Open),
    add_to_heap(Open, F0, [Start, [Start], G0], Open1),
    a_star_loop(Open1, Goal, [], Path, Distance).

/* 
   a_star_loop(+OpenHeap, +GoalNode, +ClosedSet, -Path, -Distance)
   The main loop for the A* algorithm.
*/
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

/* =============================================================================
   Random Float Definition
   =============================================================================
*/

/* 
   random_float(-R)
   Generates a random floating-point number between 0.0 and 1.0.
   
   Example:
   ?- random_float(R).
   R = 0.576839.
*/
:- if(\+ current_predicate(random_float/1)).
random_float(R) :-
    random(0.0, 1.0, R).
:- endif.

/* =============================================================================
   Delay Assignment
   =============================================================================
*/

/* 
   assign_delays(+Path, +DelayProbability, -DelayedEdges)
   Assigns delays to edges in the given Path based on DelayProbability.
   
   Parameters:
   - Path: List of node identifiers representing the path.
   - DelayProbability: Probability (between 0 and 1) that an edge will have a delay.
   - DelayedEdges: List of [Node1, Node2] pairs where delays have been assigned.
   
   Example:
   ?- assign_delays([1,2,3,4], 0.25, DelayedEdges).
   DelayedEdges = [[2, 3]].
*/
assign_delays(Path, DelayProbability, DelayedEdges) :-
    findall([U, V],
            (   adjacent(U, V, Path),
                random_float(R),
                R < DelayProbability
            ),
            DelayedEdges).

/* =============================================================================
   Helper Predicates
   =============================================================================
*/

/* 
   adjacent(+U, +V, +Path)
   Succeeds if U and V are adjacent nodes in Path.
   
   Parameters:
   - U: First node identifier.
   - V: Second node identifier.
   - Path: List of node identifiers.
   
   Example:
   ?- adjacent(2, 3, [1,2,3,4]).
   true.
*/
adjacent(U, V, Path) :-
    append(_, [U, V | _], Path).

/* =============================================================================
   Example Use Cases
   =============================================================================
*/

/* 
   Example 1: Assign delays to edges in the path [1,2,3,4] with a 25% probability.
   ?- assign_delays([1,2,3,4], 0.25, DelayedEdges).
   Possible Output:
   DelayedEdges = [[2, 3]].

   Example 2: Assign delays to edges in the path [5,6,7,8,9] with a 50% probability.
   ?- assign_delays([5,6,7,8,9], 0.5, DelayedEdges).
   Possible Output:
   DelayedEdges = [[5, 6], [7, 8]].

   Example 3: Assign delays to edges in the path [10,11,12] with a 0.0 probability (no delays).
   ?- assign_delays([10,11,12], 0.0, DelayedEdges).
   DelayedEdges = [].
*/

