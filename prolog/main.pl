% main.pl

% Include other Prolog files
:- [algorithms].
:- [utilities].

% Dynamic Declarations
:- dynamic edge/3.  % edge(Node1, Node2, Weight)
:- dynamic node/3.  % node(Name, Lat, Lon)
:- dynamic mode/2.

% Default Transportation Mode
mode(car, 60).  % Speed in km/h

% Travel Time Calculation
travel_time(Distance, Mode, Time) :-
    mode(Mode, Speed),
    Time is Distance / Speed.

% Note: Since facts are being asserted from Python, no need to include facts.pl
