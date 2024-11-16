from pyswip import Prolog
import folium
import osmnx as ox
from geopy.distance import geodesic
import matplotlib.pyplot as plt
from matplotlib.widgets import Button
from mpl_toolkits.mplot3d import Axes3D

class Map:
    def __init__(self, start_location, end_location):
        self.start_location = start_location
        self.end_location = end_location
        self.distance_km = geodesic(start_location, end_location).km
        self.buffer_dist = self.distance_km * 2000  # Buffer distance for larger area
        self.prolog = Prolog()  # Initialize Prolog interface
        self.G = self.generate_graph()
        self.route = self.get_route(self.G)

    def __str__(self):
        return f"""
            Map: Start Location: {self.start_location}
            End Location: {self.end_location}
            Distance: {self.distance_km} km
            Buffer Distance: {self.buffer_dist} m
            Total Nodes: {len(self.G.nodes)}
            Total Edges: {len(self.G.edges)}
            Shortest Route: {self.route}
            Total Distance: {self.display_distance()}
            Total Travel Time: {self.display_travel_time()}
        """
    
    def convert_time(self, seconds):
        return f"{seconds // 3600} hours {seconds // 60} minutes {seconds % 60:.2f} seconds"

    def convert_distance(self, meters):
        return f"{meters / 1000:.2f} km"

    def get_midpoint(self):
        return (
            (self.start_location[0] + self.end_location[0]) / 2,
            (self.start_location[1] + self.end_location[1]) / 2
        )

    def get_route(self, G):
        orig_node = ox.distance.nearest_nodes(G, X=self.start_location[1], Y=self.start_location[0])
        dest_node = ox.distance.nearest_nodes(G, X=self.end_location[1], Y=self.end_location[0])
        route = ox.shortest_path(G, orig_node, dest_node, weight='travel_time')
        return route

    def generate_graph(self):
        G = ox.graph_from_point(self.get_midpoint(), dist=self.buffer_dist, network_type='all') # drive, walk, bike, all
        G = ox.add_edge_speeds(G, fallback=50)  # Set a fallback speed (e.g., 50 km/h)
        G = ox.add_edge_travel_times(G)
        return G
    
    def display_travel_time(self):
        travel_time = sum(self.G[u][v][0]['travel_time'] for u, v in zip(self.route[:-1], self.route[1:]))
        travel_time = self.convert_time(travel_time)
        return travel_time
    
    def display_distance(self):
        distance = sum(self.G[u][v][0]['length'] for u, v in zip(self.route[:-1], self.route[1:]))
        distance = self.convert_distance(distance)
        return distance

    def create_map_with_folium(self, filename="map_with_folium.html"):

        
        m = folium.Map(location=self.start_location, zoom_start=20)
        folium.PolyLine(
            locations=[(self.G.nodes[node]['y'], self.G.nodes[node]['x']) for node in self.route],
            color="purple", weight=5, opacity=0.8
        ).add_to(m)
        folium.Marker(self.start_location, popup="Start").add_to(m)
        folium.Marker(self.end_location, popup="End").add_to(m)
        
        m.save(filename)

        # Adjust the function for displaying maps
    
    def create_map_without_folium(self):    
        fig, ax = plt.subplots(figsize=(8, 8))
        ox.plot_graph(self.G, ax=ax, node_color='skyblue', edge_color='gray', node_size=15, show=False, close=False)

        route_latlng = [(self.G.nodes[node]['y'], self.G.nodes[node]['x']) for node in self.route]
        ax.plot(
            [point[1] for point in route_latlng],
            [point[0] for point in route_latlng],
            linewidth=3, color="purple", label="Route"
        )
        ax.plot(self.start_location[1], self.start_location[0], marker='o', color='green', markersize=10, label="Start")
        ax.plot(self.end_location[1], self.end_location[0], marker='o', color='red', markersize=10, label="End")

        # Annotate edges with distance, speed, and travel time
        for u, v in zip(self.route[:-1], self.route[1:]):
            edge_data = self.G[u][v][0]  # Access edge attributes
            mid_x = (self.G.nodes[u]['x'] + self.G.nodes[v]['x']) / 2
            mid_y = (self.G.nodes[u]['y'] + self.G.nodes[v]['y']) / 2

            distance = edge_data.get('length', 0)  # Distance in meters
            speed = edge_data.get('speed_kph', 0)  # Speed in km/h
            travel_time = edge_data.get('travel_time', 0)  # Travel time in seconds

            annotation = (
                f"{distance:.0f} m, {speed:.0f} km/h, {travel_time:.0f} s"
            )
            ax.text(mid_x, mid_y, annotation, fontsize=8, color="blue", ha="center")

        ax.set_title("Route with Distance, Speed, and Travel Time")
        ax.legend()

        # Enable interactive zoom and pan
        plt.gca().set_aspect('auto', adjustable='datalim')
        plt.tight_layout()
        plt.show()

    def get_all_edges(self):
        edges = [(u, v) for u, v, k in self.G.edges(keys=True)]
        for u, v in edges:
            self.prolog.assertz(f"edge({u}, {v})")
        return edges

    def get_all_nodes(self):
        nodes = list(self.G.nodes())
        for node in nodes:
            lat, lon = self.G.nodes[node]['y'], self.G.nodes[node]['x']
            self.prolog.assertz(f"node({node}, {lat}, {lon})")
        return nodes

    def get_all_weights(self):
        weights = [(u, v, data['travel_time']) for u, v, data in self.G.edges(data=True) if 'travel_time' in data]
        for u, v, travel_time in weights:
            self.prolog.assertz(f"weight({u}, {v}, {travel_time})")
        return weights

