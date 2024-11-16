# map.py

from geopy.distance import geodesic
import osmnx as ox


class Map:
    def __init__(self, start_location, end_location):
        self.start_location = start_location
        self.end_location = end_location
        self.distance_km = geodesic(start_location, end_location).km
        self.buffer_dist = self.distance_km * 2000  # Buffer distance for larger area
        self.G = self.generate_graph()
        self.route = []  # Initialize the route

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
        G = ox.graph_from_point(
            self.get_midpoint(), dist=self.buffer_dist, network_type="drive"
        )
        G = ox.add_edge_speeds(G, fallback=50)  # Set a fallback speed (e.g., 50 km/h)
        G = ox.add_edge_travel_times(G)
        return G

    def create_map_without_folium(self):
        # Optional: Implement this method if needed
        pass
