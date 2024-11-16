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

