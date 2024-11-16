% main.pl
%
% This is the main Prolog file that includes other Prolog files and sets up dynamic predicates.
% It defines dynamic predicates for nodes, edges, transportation modes, and delayed edges.
% It also includes the algorithms for shortest path computation and utility predicates.

% Include other Prolog files
:- [algorithms].
:- [utilities].

% Dynamic Declarations
:- dynamic edge/3.  % edge(Node1, Node2, Weight)
:- dynamic node/3.  % node(Name, Lat, Lon)
:- dynamic mode/2.
:- dynamic delayed_edge/2.  % delayed_edge(Node1, Node2)

% Default Transportation Mode
% mode(Mode, Speed in km/h)
mode(car, 60).  % Speed in km/h

% Travel Time Calculation
% travel_time(Distance, Mode, Time)
% Calculates the travel time based on distance and transportation mode
travel_time(Distance, Mode, Time) :-
    mode(Mode, Speed),
    Time is Distance / Speed.
