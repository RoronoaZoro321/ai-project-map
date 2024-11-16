% facts.pl

% =========================
% Dynamic Declarations
% =========================

:- dynamic edge/4.
:- dynamic node/3.
:- dynamic mode/2.

% =========================
% Map Representation
% =========================

% edge(Node1, Node2, Distance, Delay).

% Initialize edges
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
% Node Positions
% =========================

% node(Name, X, Y).

reset_nodes :-
    retractall(node(_, _, _)),
    assert(node(a, 0, 0)),
    assert(node(b, 5, 0)),
    assert(node(c, 5, 5)),
    assert(node(d, 10, 5)),
    assert(node(e, 10, 10)),
    assert(node(f, 15, 10)).

% Initialize nodes
:- reset_nodes.

% =========================
% Transportation Modes
% =========================

% mode(Mode, SpeedInKmPerHour).

reset_modes :-
    retractall(mode(_, _)),
    assert(mode(car, 60)),        % Car: 60 km/h
    assert(mode(walking, 5)),     % Walking: 5 km/h
    assert(mode(motorcycle, 80)), % Motorcycle: 80 km/h
    assert(mode(airplane, 800)).  % Airplane: 800 km/h

% Initialize modes
:- reset_modes.

% =========================
% Reset Functions
% =========================

% reset_all/0
% Resets the edges, nodes, and transportation modes to their initial states.

reset_all :-
    reset_edges,
    reset_nodes,
    reset_modes.
