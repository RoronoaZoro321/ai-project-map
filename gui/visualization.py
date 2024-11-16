from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
import osmnx as ox


class Visualization:
    def __init__(self, root):
        self.root = root
        self.canvas = None
        self.ax = None  # Initialize ax as None
        self.line = None
        self.point_plot = None
        self.x_data = []
        self.y_data = []
        self.segment_colors = []

    def setup_visualization(self, map_instance):
        """
        Sets up the visualization for traversal.
        """
        # Set up the figure and axis
        fig = plt.Figure(figsize=(8, 8))
        self.ax = fig.add_subplot(111)  # Assign ax as an instance variable

        # Clear previous canvas if any
        if self.canvas:
            self.canvas.get_tk_widget().destroy()
            self.canvas = None

        self.canvas = FigureCanvasTkAgg(fig, master=self.root)
        self.canvas.get_tk_widget().grid(row=12, column=0, columnspan=2)

        # Plot the graph
        ox.plot_graph(
            map_instance.G,
            ax=self.ax,
            node_color="skyblue",
            edge_color="gray",
            node_size=15,
            show=False,
            close=False,
        )

        if not map_instance.route:
            print("No route to display.")
            return

        route_latlng = [
            (map_instance.G.nodes[node]["y"], map_instance.G.nodes[node]["x"])
            for node in map_instance.route
        ]

        # Plot start and end points
        self.ax.plot(
            map_instance.start_location[1],
            map_instance.start_location[0],
            marker="o",
            color="green",
            markersize=10,
            label="Start",
        )
        self.ax.plot(
            map_instance.end_location[1],
            map_instance.end_location[0],
            marker="o",
            color="red",
            markersize=10,
            label="End",
        )

        # Initialize the line and point plots
        (self.line,) = self.ax.plot([], [], color="purple", linewidth=3, label="Route")
        (self.point_plot,) = self.ax.plot(
            [], [], marker="o", color="blue", markersize=8
        )

        # Prepare data for animation
        self.x_data = [point[1] for point in route_latlng]
        self.y_data = [point[0] for point in route_latlng]

        # Variables to track traversal progress
        self.current_index = 0  # Index in the route
        self.elapsed_time = 0  # Total elapsed time

        # Variables to store colors for each segment
        self.segment_colors = ["purple"] * (len(map_instance.route) - 1)

        self.ax.set_title("Real-Time Route Traversal")
        self.ax.legend()
        self.canvas.draw()

        # Save original edges in case we need to restore them
        map_instance.original_edges = {}
        for u, v, data in map_instance.G.edges(data=True):
            map_instance.original_edges[(u, v)] = data

    def update_visualization(self, x, y, color):
        """
        Updates the traversal marker and path.
        """
        self.point_plot.set_data(x, y)
        self.line.set_data(x, y)
        self.line.set_color(color)
        self.canvas.draw()
