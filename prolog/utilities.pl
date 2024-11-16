/* =============================================================================
   utilities.pl

   Description:
   -----------
   This file contains utility predicates used by the shortest path algorithms, such
   as heuristic functions for the A* algorithm. It includes calculations for
   estimating the distance between nodes using the Haversine formula.

   =============================================================================
*/

/* =============================================================================
   Dynamic Predicate Declarations
   =============================================================================
*/
:- dynamic edge/3.          % edge(Node1, Node2, Weight)
:- dynamic node/3.          % node(Name, Lat, Lon)

/* =============================================================================
   Heuristic Function for A* Algorithm
   =============================================================================
*/

/* 
   heuristic(+Node1, +Node2, -HeuristicValue)
   Calculates the Haversine distance between Node1 and Node2 as the heuristic.
   
   Parameters:
   - Node1: The first node identifier.
   - Node2: The second node identifier.
   - HeuristicValue: The calculated heuristic distance in kilometers.
   
   Example:
   ?- heuristic(1, 2, H).
   H = 5.3.
*/
heuristic(Node1, Node2, HeuristicValue) :-
    node(Node1, Lat1, Lon1),
    node(Node2, Lat2, Lon2),
    haversine_distance(Lat1, Lon1, Lat2, Lon2, HeuristicValue).

/* =============================================================================
   Haversine Distance Calculation
   =============================================================================
*/

/* 
   haversine_distance(+Lat1, +Lon1, +Lat2, +Lon2, -Distance)
   Calculates the Haversine distance between two latitude-longitude points.
   
   Parameters:
   - Lat1, Lon1: Latitude and Longitude of the first point in degrees.
   - Lat2, Lon2: Latitude and Longitude of the second point in degrees.
   - Distance: The Haversine distance in kilometers.
   
   Example:
   ?- haversine_distance(52.5200, 13.4050, 48.8566, 2.3522, D).
   D = 878.7.
*/
haversine_distance(Lat1, Lon1, Lat2, Lon2, Distance) :-
    DegreesToRadians is pi / 180,
    DLat is (Lat2 - Lat1) * DegreesToRadians,
    DLon is (Lon2 - Lon1) * DegreesToRadians,
    A is sin(DLat / 2) ** 2 + cos(Lat1 * DegreesToRadians) * cos(Lat2 * DegreesToRadians) * sin(DLon / 2) ** 2,
    C is 2 * atan2(sqrt(A), sqrt(1 - A)),
    EarthRadiusKm = 6371,  % Earth's radius in kilometers
    Distance is EarthRadiusKm * C.

/* =============================================================================
   Example Use Cases
   =============================================================================
*/

/* 
   Example 1: Calculate heuristic between node 1 and node 2.
   Assuming node(1, 52.5200, 13.4050) and node(2, 48.8566, 2.3522) are defined.
   ?- heuristic(1, 2, H).
   H = 878.7.

   Example 2: Calculate Haversine distance between Berlin (52.5200, 13.4050) and Paris (48.8566, 2.3522).
   ?- haversine_distance(52.5200, 13.4050, 48.8566, 2.3522, D).
   D = 878.7.

   Example 3: Calculate heuristic between node 3 and node 4.
   Assuming node(3, 34.0522, -118.2437) and node(4, 36.1699, -115.1398) are defined.
   ?- heuristic(3, 4, H).
   H = 367.6.
*/
