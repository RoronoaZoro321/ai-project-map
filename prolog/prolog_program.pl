% prolog_program.pl

% =========================
% Dynamic Declarations
% =========================

:- dynamic edge/4.
:- dynamic mode/2.

% =========================
% Map Representation
% =========================

% edge(Node1, Node2, Distance, Delay).
% Define the edges of the graph with distances between nodes and delays.
% Edges are defined in both directions to represent an undirected graph.
% Delay is initialized to 0 for all edges.

reset_edges :-
    retractall(edge(_, _, _, _)),
    % Re-define the edges in both directions with Delay = 0
    assert(edge(a, b, 5, 0)), assert(edge(b, a, 5, 0)),
    assert(edge(a, c, 10, 0)), assert(edge(c, a, 10, 0)),
    assert(edge(b, c, 2, 0)), assert(edge(c, b, 2, 0)),
    assert(edge(b, d, 3, 0)), assert(edge(d, b, 3, 0)),
    assert(edge(c, d, 1, 0)), assert(edge(d, c, 1, 0)),
    assert(edge(c, e, 7, 0)), assert(edge(e, c, 7, 0)),
    assert(edge(d, e, 2, 0)), assert(edge(e, d, 2, 0)),
    assert(edge(d, f, 3, 0)), assert(edge(f, d, 3, 0)),
    assert(edge(e, f, 1, 0)), assert(edge(f, e, 1, 0)).

% Initialize edges
:- reset_edges.

% =========================
% Transportation Modes
% =========================

% mode(Mode, SpeedInKmPerHour).
% Define the speed for each mode of transportation.

reset_modes :-
    retractall(mode(_, _)),
    assert(mode(car, 60)),        % Car: 60 km/h
    assert(mode(walking, 5)),     % Walking: 5 km/h
    assert(mode(motorcycle, 80)), % Motorcycle: 80 km/h
    assert(mode(airplane, 800)).  % Airplane: 800 km/h

% Initialize modes
:- reset_modes.

% =========================
% Travel Time Calculation
% =========================

% travel_time(Distance, Mode, Time).
% Calculate travel time based on distance and mode of transportation.

travel_time(Distance, Mode, Time) :-
    mode(Mode, Speed),
    Time is Distance / Speed.

% =========================
% Dijkstra's Algorithm
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
% Obstacle Handling
% =========================

% remove_edge(Node1, Node2)
% Removes an edge between Node1 and Node2 to simulate an obstacle.
% Succeeds if the edge exists, fails otherwise.

remove_edge(Node1, Node2) :-
    (   retract(edge(Node1, Node2, _, _)),
        retract(edge(Node2, Node1, _, _))
    ->  format('Obstacle introduced between ~w and ~w~n', [Node1, Node2])
    ;   format('No edge exists between the specified nodes.~n'),
        fail
    ).

% =========================
% Delay Handling
% =========================

% add_delay(Node1, Node2, AdditionalDelay)
% Adds a delay to the edge between Node1 and Node2.
% Succeeds if the edge exists, fails otherwise.

add_delay(Node1, Node2, AdditionalDelay) :-
    (   retract(edge(Node1, Node2, Distance, CurrentDelay)),
        NewDelay is CurrentDelay + AdditionalDelay,
        assert(edge(Node1, Node2, Distance, NewDelay)),
        retract(edge(Node2, Node1, Distance, _)),
        assert(edge(Node2, Node1, Distance, NewDelay)),
        format('Delay of ~w added between ~w and ~w~n', [AdditionalDelay, Node1, Node2])
    ->  true
    ;   format('No edge exists between the specified nodes.~n'),
        fail
    ).

% remove_delay(Node1, Node2)
% Removes any delay on the edge between Node1 and Node2.

remove_delay(Node1, Node2) :-
    (   retract(edge(Node1, Node2, Distance, _)),
        assert(edge(Node1, Node2, Distance, 0)),
        retract(edge(Node2, Node1, Distance, _)),
        assert(edge(Node2, Node1, Distance, 0)),
        format('Delay removed between ~w and ~w~n', [Node1, Node2])
    ->  true
    ;   format('No edge exists between the specified nodes.~n'),
        fail
    ).

% =========================
% Adding Nodes and Edges
% =========================

% add_edge(Node1, Node2, Distance)
% Adds an edge between Node1 and Node2 with the specified Distance.

add_edge(Node1, Node2, Distance) :-
    \+ edge(Node1, Node2, _, _),
    assert(edge(Node1, Node2, Distance, 0)),
    assert(edge(Node2, Node1, Distance, 0)),
    format('Edge added between ~w and ~w with distance ~w~n', [Node1, Node2, Distance]).
add_edge(Node1, Node2, _) :-
    edge(Node1, Node2, _, _),
    format('Edge already exists between ~w and ~w~n', [Node1, Node2]).

% =========================
% Reset Functions
% =========================

% reset_all/0
% Resets the edges and transportation modes to their initial states.

reset_all :-
    reset_edges,
    reset_modes.

% =========================
% Main Predicate
% =========================

% find_route(StartNode, EndNode, Mode, Path, TotalDistance, TotalTime).
% Finds the shortest path and calculates travel time based on the mode.

find_route(Start, Goal, Mode, Path, Distance, Time) :-
    % Ensure the mode is valid
    mode(Mode, _),
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
    findall((N1, N2, D, Delay), edge(N1, N2, D, Delay), Edges),
    format('Current graph edges:~n'),
    forall(member((Node1, Node2, Distance, Delay), Edges),
          format('Edge from ~w to ~w with distance ~w and delay ~w~n', [Node1, Node2, Distance, Delay])).

% =========================
% Example Usage
% =========================

/*
Examples:

?- find_route(a, f, car, Path, Distance, Time).
Path = [a, b, c, d, e, f],
Distance = 13,
Time = 0.21666666666666667.

?- add_delay(c, d, 5).
Delay of 5 added between c and d

?- find_route(a, f, car, Path, Distance, Time).
Path = [a, b, d, e, f],
Distance = 11,
Time = 0.18333333333333332.

?- remove_delay(c, d).
Delay removed between c and d

?- reset_all.
% Resets the graph and transportation modes to initial state.
*/
