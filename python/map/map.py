from pyswip import Prolog
import folium
import osmnx as ox
from geopy.distance import geodesic
import matplotlib.pyplot as plt

class Map:
    def __init__(self, start_location, end_location):
        self.start_location = start_location
        self.end_location = end_location
        self.distance_km = geodesic(start_location, end_location).km
        self.buffer_dist = self.distance_km * 500  # Buffer distance for larger area
        self.prolog = Prolog()  # Initialize Prolog interface
        self.G = self.generate_graph()
        self.route = self.get_route(self.G)

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
        G = ox.graph_from_point(self.get_midpoint(), dist=self.buffer_dist, network_type='drive')
        G = ox.add_edge_speeds(G)
        G = ox.add_edge_travel_times(G)
        return G


    def create_map_with_folium(self, filename="map_with_folium.html"):

        m = folium.Map(location=self.start_location, zoom_start=10)
        folium.PolyLine(
            locations=[(self.G.nodes[node]['y'], self.G.nodes[node]['x']) for node in self.route],
            color="purple", weight=5, opacity=0.8
        ).add_to(m)
        folium.Marker(self.start_location, popup="Start").add_to(m)
        folium.Marker(self.end_location, popup="End").add_to(m)
        
        m.save(filename)

    def create_map_without_folium(self):

        fig, ax = plt.subplots(figsize=(10, 10))
        ox.plot_graph(self.G, ax=ax, node_color='skyblue', edge_color='gray', node_size=15, show=False, close=False)

        route_latlng = [(self.G.nodes[node]['y'], self.G.nodes[node]['x']) for node in self.route]
        ax.plot(
            [point[1] for point in route_latlng],
            [point[0] for point in route_latlng],
            linewidth=3, color="purple", label="Route"
        )
        ax.plot(self.start_location[1], self.start_location[0], marker='o', color='green', markersize=10, label="Start")
        ax.plot(self.end_location[1], self.end_location[0], marker='o', color='red', markersize=10, label="End")
        ax.set_title("Route from Start to End")
        ax.legend()
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

# # Generate road network
# G = ox.graph_from_point(map_instance.get_midpoint(), dist=map_instance.buffer_dist, network_type='drive')
# G = ox.add_edge_speeds(G)
# G = ox.add_edge_travel_times(G)

# # Retrieve all edges, nodes, and weights and assert them as facts in Prolog
# edges = map_instance.get_all_edges(G)
# nodes = map_instance.get_all_nodes(G)
# weights = map_instance.get_all_weights(G)

# # Check facts by querying Prolog
# edge_query_result = list(map_instance.prolog.query("edge(X, Y)"))
# node_query_result = list(map_instance.prolog.query("node(X, Lat, Lon)"))
# weight_query_result = list(map_instance.prolog.query("weight(X, Y, TravelTime)"))

# print("Edges:", edge_query_result)
# print("Nodes:", node_query_result)
# print("Weights:", weight_query_result)

