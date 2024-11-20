/* =============================================================================
   main.pl

   Description:
   -----------
   This is the main Prolog file that initializes the shortest path finder application.
   It consults the necessary modules, declares dynamic predicates, defines transportation
   modes with their respective speeds, and provides a predicate to calculate travel time
   based on distance and mode of transportation.
   
   =============================================================================
*/

/* =============================================================================
   Module Imports
   =============================================================================
*/
:- [algorithms].
:- [utilities].

/* =============================================================================
   Dynamic Predicate Declarations
   =============================================================================
*/
:- dynamic edge/3.          % edge(Node1, Node2, Weight)
:- dynamic node/3.          % node(Name, Lat, Lon)
:- dynamic mode/2.          % mode(TransportationMode, SpeedKPH)
:- dynamic delayed_edge/2.  % delayed_edge(Node1, Node2)

/* =============================================================================
   Transportation Modes and Their Speeds (in km/h)
   =============================================================================
*/
/*
    mode(TransportationMode, SpeedKPH)
    Defines the speed for each transportation mode.
*/
mode(car, 60).          % Speed for Car
mode(fast_car, 120).    % Speed for Fast Car
mode(walking, 5).       % Speed for Walking
mode(motorcycle, 40).   % Speed for Motorcycle

/* =============================================================================
   Travel Time Calculation
   =============================================================================
*/
% travel_time(+Distance, +Mode, -Time)
% Calculates the travel time based on distance and transportation mode.
% 
% Parameters:
% - Distance: Distance to travel in kilometers.
% - Mode: Transportation mode (e.g., car, walking, motorcycle, fast_car).
% - Time: Calculated travel time in hours.
travel_time(Distance, Mode, Time) :-
    mode(Mode, Speed),
    Time is Distance / Speed.

/* =============================================================================
   Example Use Cases
   =============================================================================
*/

/* 
    Example 1: Calculate travel time for 120 km by fast_car.
    ?- travel_time(120, fast_car, Time).
    Time = 1.0.
    
    Example 2: Calculate travel time for 10 km fast_car.
    ?- travel_time(10, fast_car, Time).
    Time = 0.08333333333333333.
*/
