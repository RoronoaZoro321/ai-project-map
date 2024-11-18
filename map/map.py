# map/map.py

from geopy.distance import geodesic
import osmnx as ox
import warnings


class Map:
    def __init__(self, start_location, end_location, speed_kph=50):
        """
        Initializes the Map instance with start and end locations and transportation speed.

        Args:
            start_location (tuple): (latitude, longitude) of the start location.
            end_location (tuple): (latitude, longitude) of the end location.
            speed_kph (float): Speed in km/h based on transportation mode.
        """
        self.start_location = start_location  # (lat, lon)
        self.end_location = end_location  # (lat, lon)
        self.distance_km = geodesic(start_location, end_location).km
        self.buffer_dist = max(
            self.distance_km * 2000, 1000
        )  # Ensure a minimum buffer distance of 1000 meters
        self.G = self.generate_graph(speed_kph)
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
        self.delayed_edges = []  # List of edges with delays

    def generate_graph(self, speed_kph):
        """
        Generates the road network graph based on the midpoint and buffer distance.

        Args:
            speed_kph (float): Speed in km/h to set for all edges.

        Returns:
            networkx.MultiDiGraph: The generated road network graph.
        """
        try:
            # Suppress FutureWarnings related to bbox coordinate order
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", FutureWarning)
                G = ox.graph_from_point(
                    self.get_midpoint(), dist=self.buffer_dist, network_type="drive"
                )

            # Set all edges' speed to the selected transportation mode's speed
            for u, v, k, data in G.edges(keys=True, data=True):
                data["speed_kph"] = speed_kph  # Override speed with selected mode
            # Recalculate travel times based on the new speed
            G = ox.add_edge_travel_times(G)
            return G
        except Exception as e:
            raise ValueError(f"Failed to generate graph: {e}")

    def get_midpoint(self):
        return (
            (self.start_location[0] + self.end_location[0]) / 2,
            (self.start_location[1] + self.end_location[1]) / 2,
        )
