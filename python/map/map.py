import folium
import osmnx as ox
from geopy.distance import geodesic
import matplotlib.pyplot as plt

class Map:
    def __init__(self, start_location, end_location):
        """
        Initialize the Map class with start and end locations.

        Parameters:
        start_location (tuple): Latitude and longitude of the starting location.
        end_location (tuple): Latitude and longitude of the ending location.
        """
        self.start_location = start_location
        self.end_location = end_location
        self.distance_km = geodesic(start_location, end_location).km
        self.buffer_dist = self.distance_km * 500  # Buffer distance for larger area

    def get_midpoint(self):
        """
        Calculate the midpoint between the start and end locations.
        """
        return (
            (self.start_location[0] + self.end_location[0]) / 2,
            (self.start_location[1] + self.end_location[1]) / 2
        )

    def get_route(self, G):
        """
        Get the shortest path by travel time between the start and end locations.
        """
        # Find nearest nodes to the start and end locations
        orig_node = ox.distance.nearest_nodes(G, X=self.start_location[1], Y=self.start_location[0])
        dest_node = ox.distance.nearest_nodes(G, X=self.end_location[1], Y=self.end_location[0])
        
        # Get the shortest path by travel time
        route = ox.shortest_path(G, orig_node, dest_node, weight='travel_time')
        
        return route

    def create_map_with_folium(self):
        """
        Create a folium map showing the road route between the start and end locations.
        """
        # Generate road network
        G = ox.graph_from_point(self.get_midpoint(), dist=self.buffer_dist, network_type='drive')
        G = ox.add_edge_speeds(G)
        G = ox.add_edge_travel_times(G)
        
        # Get the route
        route = self.get_route(G)

        # Create a folium map
        m = folium.Map(location=self.start_location, zoom_start=10)

        # Plot the route on the map
        folium.PolyLine(
            locations=[(G.nodes[node]['y'], G.nodes[node]['x']) for node in route],
            color="purple", weight=5, opacity=0.8
        ).add_to(m)

        # Add markers for start and end points
        folium.Marker(self.start_location, popup="Start").add_to(m)
        folium.Marker(self.end_location, popup="End").add_to(m)
        
        return m

    def create_map_without_folium(self):
        """
        Create a static map using matplotlib to display the route.
        """
        # Generate road network
        G = ox.graph_from_point(self.get_midpoint(), dist=self.buffer_dist, network_type='drive')
        G = ox.add_edge_speeds(G)
        G = ox.add_edge_travel_times(G)
        
        # Get the route
        route = self.get_route(G)

        # Plot the graph and the route
        fig, ax = plt.subplots(figsize=(10, 10))
        # Plot the graph in the background
        ox.plot_graph(G, ax=ax, node_color='skyblue', edge_color='gray', node_size=15, show=False, close=False)

        # Plot the route on the map
        route_latlng = [(G.nodes[node]['y'], G.nodes[node]['x']) for node in route]
        ax.plot(
            [point[1] for point in route_latlng],
            [point[0] for point in route_latlng],
            linewidth=3, color="purple", label="Route"
        )

        # Add markers for the start and end points
        ax.plot(self.start_location[1], self.start_location[0], marker='o', color='green', markersize=10, label="Start")
        ax.plot(self.end_location[1], self.end_location[0], marker='o', color='red', markersize=10, label="End")
        
        # Add title and legend
        ax.set_title("Route from Start to End")
        ax.legend()
        
        # Display the map
        plt.show()

# Usage
start_location = (37.7749, -122.4194)  # San Francisco
end_location = (37.8, -122.427)  # Near San Francisco

# Create Map instance
map_instance = Map(start_location, end_location)

# Create a map with folium and save it as an HTML file
map_with_folium = map_instance.create_map_with_folium()
map_with_folium.save("map_with_folium.html")

# Create a static map without folium
map_instance.create_map_without_folium()

