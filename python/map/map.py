# map.py

from geopy.distance import geodesic
import matplotlib.pyplot as plt
import osmnx as ox
from pyswip import Prolog


class Map:
    def __init__(self, start_location, end_location):
        self.start_location = start_location
        self.end_location = end_location
        self.distance_km = geodesic(start_location, end_location).km
        self.buffer_dist = self.distance_km * 2000  # Buffer distance for larger area
        self.G = self.generate_graph()
        self.route = []  # Initialize the route
        # Initialize Prolog if needed
        # self.prolog = Prolog()
        # self.prolog.consult("main.pl")

    def __str__(self):
        return f"""
            Map: Start Location: {self.start_location}
            End Location: {self.end_location}
            Distance: {self.distance_km:.2f} km
            Buffer Distance: {self.buffer_dist:.2f} m
            Total Nodes: {len(self.G.nodes)}
            Total Edges: {len(self.G.edges)}
        """

    def convert_time(self, seconds):
        hours = int(seconds // 3600)
        minutes = int((seconds % 3600) // 60)
        secs = seconds % 60
        return f"{hours} hours {minutes} minutes {secs:.2f} seconds"

    def convert_distance(self, meters):
        return f"{meters / 1000:.2f} km"

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

    def create_map_with_folium(self, filename="map_with_folium.html"):
        import folium

        # Create the base map
        m = folium.Map(location=self.start_location, zoom_start=15)

        # Add start and end markers
        folium.Marker(
            self.start_location, popup="Start", icon=folium.Icon(color="green")
        ).add_to(m)
        folium.Marker(
            self.end_location, popup="End", icon=folium.Icon(color="red")
        ).add_to(m)

        if not self.route:
            print("No route to display.")
            return

        # Get the route coordinates
        route_latlng = [
            (self.G.nodes[node]["y"], self.G.nodes[node]["x"]) for node in self.route
        ]

        # Add the entire route as a line (optional)
        folium.PolyLine(
            locations=route_latlng, color="gray", weight=5, opacity=0.5
        ).add_to(m)

        # Add a marker that will move along the route
        moving_marker = folium.Marker(location=self.start_location)
        moving_marker.add_to(m)

        # JavaScript to animate the marker
        steps = len(route_latlng)
        delay = 1000  # milliseconds between steps

        # Build the JavaScript code
        latlngs_str = str(route_latlng)
        js = f"""
        var latlngs = {latlngs_str};
        var marker = {moving_marker.get_name()};
        var delay = {delay};

        function animateMarker(index) {{
            if (index >= latlngs.length) return;
            marker.setLatLng(latlngs[index]);
            setTimeout(function() {{
                animateMarker(index + 1);
            }}, delay);
        }}

        animateMarker(0);
        """

        # Add the JavaScript to the map
        m.get_root().script.add_child(folium.Element(js))

        # Save the map
        m.save(filename)

    def create_map_without_folium(self):
        import matplotlib.animation as animation

        fig, ax = plt.subplots(figsize=(8, 8))
        ox.plot_graph(
            self.G,
            ax=ax,
            node_color="skyblue",
            edge_color="gray",
            node_size=15,
            show=False,
            close=False,
        )

        if not self.route:
            print("No route to display.")
            return

        route_latlng = [
            (self.G.nodes[node]["y"], self.G.nodes[node]["x"]) for node in self.route
        ]

        # Plot start and end points
        ax.plot(
            self.start_location[1],
            self.start_location[0],
            marker="o",
            color="green",
            markersize=10,
            label="Start",
        )
        ax.plot(
            self.end_location[1],
            self.end_location[0],
            marker="o",
            color="red",
            markersize=10,
            label="End",
        )

        # Prepare data for animation
        x_data = [point[1] for point in route_latlng]
        y_data = [point[0] for point in route_latlng]

        (line,) = ax.plot([], [], color="purple", linewidth=3, label="Route")
        (point,) = ax.plot([], [], marker="o", color="blue", markersize=8)

        def init():
            line.set_data([], [])
            point.set_data([], [])
            return line, point

        def update(num):
            line.set_data(x_data[:num], y_data[:num])
            point.set_data(x_data[num - 1], y_data[num - 1])
            return line, point

        ani = animation.FuncAnimation(
            fig,
            update,
            frames=len(x_data),
            init_func=init,
            interval=500,
            blit=True,
            repeat=False,
        )

        ax.set_title("Real-Time Route Traversal")
        ax.legend()
        plt.tight_layout()
        plt.show()
