% main.pl

:- [algorithms].
:- [utilities].

% Dynamic Declarations
:- dynamic edge/3.  % edge(Node1, Node2, Weight)
:- dynamic node/3.  % node(Name, Lat, Lon)
:- dynamic mode/2.
:- dynamic delayed_edge/2.  % delayed_edge(Node1, Node2)

% Transportation Modes and Their Speeds (in km/h)
mode(car, 60).          % Speed for Car
mode(walking, 5).       % Speed for Walking
mode(motorcycle, 40).   % Speed for Motorcycle

% Travel Time Calculation
% travel_time(Distance, Mode, Time)
% Calculates the travel time based on distance and transportation mode
travel_time(Distance, Mode, Time) :-
    mode(Mode, Speed),
    Time is Distance / Speed.
