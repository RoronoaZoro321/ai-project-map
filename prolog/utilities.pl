% utilities.pl
%
% This file contains utility predicates such as the heuristic function for A* algorithm.

% Dynamic Declarations
:- dynamic edge/3.
:- dynamic node/3.

% Heuristic Function

% heuristic(+Node1, +Node2, -HeuristicValue)
% Calculates the Haversine distance between Node1 and Node2 as the heuristic
heuristic(Node1, Node2, HeuristicValue) :-
    node(Node1, Lat1, Lon1),
    node(Node2, Lat2, Lon2),
    haversine_distance(Lat1, Lon1, Lat2, Lon2, HeuristicValue).

% Haversine Distance Calculation

% haversine_distance(+Lat1, +Lon1, +Lat2, +Lon2, -Distance)
% Calculates the Haversine distance between two latitude-longitude points
haversine_distance(Lat1, Lon1, Lat2, Lon2, Distance) :-
    DegreesToRadians is pi / 180,
    DLat is (Lat2 - Lat1) * DegreesToRadians,
    DLon is (Lon2 - Lon1) * DegreesToRadians,
    A is sin(DLat / 2) ** 2 + cos(Lat1 * DegreesToRadians) * cos(Lat2 * DegreesToRadians) * sin(DLon / 2) ** 2,
    C is 2 * atan2(sqrt(A), sqrt(1 - A)),
    EarthRadiusKm = 6371,  % Earth's radius in kilometers
    Distance is EarthRadiusKm * C.
