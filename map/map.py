# map/map.py

import warnings
from geopy.distance import geodesic
import osmnx as ox


class Map:
    def __init__(self, start_location, end_location):
        self.start_location = start_location  # (lat, lon)
        self.end_location = end_location  # (lat, lon)
        self.distance_km = geodesic(start_location, end_location).km
        self.buffer_dist = max(
            self.distance_km * 2000, 1000
        )  # Ensure a minimum buffer distance of 1000 meters
        self.G = self.generate_graph()
        self.route = []  # Initialize the route
        self.distances = []  # Distance for each edge in the path
        self.times = []  # Travel time for each edge in the path
        self.cumulative_distances = [0]  # Cumulative distance
        self.cumulative_times = [0]  # Cumulative time
        self.total_distance = 0  # Total distance of the path
        self.total_time = 0  # Total travel time of the path
        self.prolog_interface = None  # To be assigned externally
        self.prolog_algorithm = None  # To be assigned externally
        self.orig_node = None
        self.dest_node = None
        self.delay_probability = 0.0
        self.original_edges = {}
        self.processed_edges = set()  # Set to track edges already evaluated for delays

    def __str__(self):
        return f"""
            Map: Start Location: {self.start_location}
            End Location: {self.end_location}
            Distance: {self.distance_km:.2f} km
            Buffer Distance: {self.buffer_dist:.2f} m
            Total Nodes: {len(self.G.nodes)}
            Total Edges: {len(self.G.edges)}
        """

    def get_midpoint(self):
        return (
            (self.start_location[0] + self.end_location[0]) / 2,
            (self.start_location[1] + self.end_location[1]) / 2,
        )

    def generate_graph(self):
        try:
            # Suppress FutureWarnings related to bbox coordinate order
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", FutureWarning)
                G = ox.graph_from_point(
                    self.get_midpoint(), dist=self.buffer_dist, network_type="drive"
                )
            G = ox.add_edge_speeds(
                G, fallback=50
            )  # Set a fallback speed (e.g., 50 km/h)
            G = ox.add_edge_travel_times(G)
            return G
        except Exception as e:
            raise ValueError(f"Failed to generate graph: {e}")

    def create_map_without_folium(self):
        # Optional: Implement this method if needed
        pass
