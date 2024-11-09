% prolog_program.pl

% =========================
% Dynamic Declarations
% =========================

:- dynamic edge/3.
:- dynamic mode_speed/2.

% =========================
% Map Representation
% =========================

% edge(Node1, Node2, Distance).
% Define the edges of the graph with distances between nodes.
% Edges are defined in both directions to represent an undirected graph.

edge(a, b, 5).
edge(b, a, 5).

edge(a, c, 10).
edge(c, a, 10).

edge(b, c, 2).
edge(c, b, 2).

edge(b, d, 3).
edge(d, b, 3).

edge(c, d, 1).
edge(d, c, 1).

edge(c, e, 7).
edge(e, c, 7).

edge(d, e, 2).
edge(e, d, 2).

edge(d, f, 3).
edge(f, d, 3).

edge(e, f, 1).
edge(f, e, 1).

% =========================
% Transportation Modes
% =========================

% mode_speed(Mode, SpeedInKmPerHour).
% Define the speed for each mode of transportation.

reset_speeds :-
    retractall(mode_speed(_, _)),
    assert(mode_speed(car, 60)),
    assert(mode_speed(walking, 5)),
    assert(mode_speed(motorcycle, 80)),
    assert(mode_speed(airplane, 800)).

% Initialize mode speeds
:- reset_speeds.

% =========================
% Travel Time Calculation
% =========================

% travel_time(Distance, Mode, Time).
% Calculate travel time based on distance and mode of transportation.

travel_time(Distance, Mode, Time) :-
    mode_speed(Mode, Speed),
    Time is Distance / Speed.

% =========================
% Dijkstras Algorithm
% =========================

% Comparator predicate for predsort/3
compare_dist(<, [_, _, Dist1], [_, _, Dist2]) :- Dist1 < Dist2.
compare_dist(=, [_, _, Dist1], [_, _, Dist2]) :- Dist1 =:= Dist2.
compare_dist(>, [_, _, Dist1], [_, _, Dist2]) :- Dist1 > Dist2.

% shortest_path(StartNode, EndNode, Path, TotalDistance).
% Find the shortest path between StartNode and EndNode.

shortest_path(Start, Goal, Path, Distance) :-
    dijkstra([[Start, [Start], 0]], Goal, RevPath, Distance),
    reverse(RevPath, Path).

% Dijkstras helper predicate using predsort/3
dijkstra([[Goal, Path, Dist] | _], Goal, Path, Dist).
dijkstra([[CurrentNode, CurrentPath, CurrentDist] | Rest], Goal, Path, Dist) :-
    findall(
        [NextNode, [NextNode | CurrentPath], NewDist],
        (
            edge(CurrentNode, NextNode, D),
            \+ member(NextNode, CurrentPath),
            NewDist is CurrentDist + D
        ),
        NextNodes
    ),
    append(Rest, NextNodes, UpdatedQueue),
    predsort(compare_dist, UpdatedQueue, SortedQueue),
    dijkstra(SortedQueue, Goal, Path, Dist).

% =========================
% Obstacle Handling
% =========================

% remove_edge(Node1, Node2)
% Removes an edge between Node1 and Node2 to simulate an obstacle.
% Succeeds if the edge exists, fails otherwise.

remove_edge(Node1, Node2) :-
    (   retract(edge(Node1, Node2, _)),
        retract(edge(Node2, Node1, _))
    ->  format('Obstacle introduced between ~w and ~w~n', [Node1, Node2])
    ;   format('No edge exists between the specified nodes.~n'),
        fail
    ).

% =========================
% Adding Nodes and Edges
% =========================

% add_edge(Node1, Node2, Distance)
% Adds an edge between Node1 and Node2 with the specified Distance.

add_edge(Node1, Node2, Distance) :-
    \+ edge(Node1, Node2, _),
    assert(edge(Node1, Node2, Distance)),
    assert(edge(Node2, Node1, Distance)),
    format('Edge added between ~w and ~w with distance ~w~n', [Node1, Node2, Distance]).
add_edge(Node1, Node2, _) :-
    edge(Node1, Node2, _),
    format('Edge already exists between ~w and ~w~n', [Node1, Node2]).

% =========================
% Reset Functions
% =========================

% reset_edges/0
% Resets the edges to their original state.

reset_edges :-
    retractall(edge(_, _, _)),
    % Re-define the edges in both directions
    assert(edge(a, b, 5)), assert(edge(b, a, 5)),
    assert(edge(a, c, 10)), assert(edge(c, a, 10)),
    assert(edge(b, c, 2)), assert(edge(c, b, 2)),
    assert(edge(b, d, 3)), assert(edge(d, b, 3)),
    assert(edge(c, d, 1)), assert(edge(d, c, 1)),
    assert(edge(c, e, 7)), assert(edge(e, c, 7)),
    assert(edge(d, e, 2)), assert(edge(e, d, 2)),
    assert(edge(d, f, 3)), assert(edge(f, d, 3)),
    assert(edge(e, f, 1)), assert(edge(f, e, 1)).

% reset_speeds/0
% Resets the transportation modes to their initial speeds.

% Already defined above

% reset_all/0
% Resets the edges and transportation modes to their initial states.

reset_all :-
    reset_edges,
    reset_speeds.

% =========================
% Main Predicate
% =========================

% find_route(StartNode, EndNode, Mode, Path, TotalDistance, TotalTime).
% Finds the shortest path and calculates travel time based on the mode.

find_route(Start, Goal, Mode, Path, Distance, Time) :-
    % Ensure the mode is valid
    mode_speed(Mode, _),
    % Find the shortest path
    shortest_path(Start, Goal, Path, Distance),
    % Calculate travel time
    travel_time(Distance, Mode, Time).

% =========================
% Utility Predicates
% =========================

% show_graph/0
% Display all the edges in the graph.

show_graph :-
    findall((N1, N2, D), edge(N1, N2, D), Edges),
    format('Current graph edges:~n'),
    forall(member((Node1, Node2, Distance), Edges),
          format('Edge from ~w to ~w with distance ~w~n', [Node1, Node2, Distance])).

% =========================
% Example Usage
% =========================

/*
To use this Prolog program, you can consult it in your Prolog interpreter or from Python using PySwip.

Examples:

?- find_route(a, f, car, Path, Distance, Time).
Path = [a, b, c, d, e, f],
Distance = 13,
Time = 0.21666666666666667.

?- remove_edge(c, d).
Obstacle introduced between c and d

?- find_route(a, f, car, Path, Distance, Time).
Path = [a, b, d, e, f],
Distance = 11,
Time = 0.18333333333333332.

?- remove_edge(c, d).
No edge exists between the specified nodes.

?- reset_all.
% Resets the graph and transportation modes to initial state.

Note: If no path is found due to obstacles, the query will fail.
*/
