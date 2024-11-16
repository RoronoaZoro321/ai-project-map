% utilities.pl

% =========================
% Travel Time Calculation
% =========================

% travel_time(Distance, Mode, Time).
% Calculate travel time based on distance and mode of transportation.

travel_time(Distance, Mode, Time) :-
    mode(Mode, Speed),
    Time is Distance / Speed.

% =========================
% Heuristic Function
% =========================

% heuristic(Node1, Node2, HeuristicValue).
% Calculate the Euclidean distance between Node1 and Node2.

heuristic(Node1, Node2, HeuristicValue) :-
    node(Node1, X1, Y1),
    node(Node2, X2, Y2),
    DX is X1 - X2,
    DY is Y1 - Y2,
    HeuristicValue is sqrt(DX*DX + DY*DY).

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
% Utility Predicates
% =========================

% show_graph/0
% Display all the edges in the graph.

show_graph :-
    findall((N1, N2, D, Delay), edge(N1, N2, D, Delay), Edges),
    format('Current graph edges:~n'),
    forall(member((Node1, Node2, Distance, Delay), Edges),
          format('Edge from ~w to ~w with distance ~w and delay ~w~n', [Node1, Node2, Distance, Delay])).
