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

% Edges will be added dynamically from the Python code.

% =========================
% Node Positions
% =========================

% node(Name, Latitude, Longitude).

% Nodes will be added dynamically from the Python code.

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
    retractall(edge(_, _, _, _)),
    retractall(node(_, _, _)),
    reset_modes.
