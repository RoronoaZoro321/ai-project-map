% main.pl

% Include other Prolog files
:- [facts].
:- [algorithms].
:- [utilities].

% =========================
% Main Predicate
% =========================

% find_route(StartNode, EndNode, Mode, Algorithm, Path, TotalDistance, TotalTime).
% Finds the shortest path using the specified algorithm and calculates travel time.

find_route(Start, Goal, Mode, Algorithm, Path, Distance, Time) :-
    % Ensure the mode is valid
    mode(Mode, _),
    % Find the shortest path using the specified algorithm
    shortest_path(Start, Goal, Algorithm, Path, Distance),
    % Calculate travel time
    travel_time(Distance, Mode, Time).
