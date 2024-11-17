# gui/visualization.py

import tkinter as tk  # Ensure tkinter is imported
from tkinter import ttk  # Import ttk for custom styling
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk
import matplotlib.pyplot as plt
import osmnx as ox


class Visualization:
    def __init__(self, parent):
        self.parent = parent
        self.canvas = None
        self.toolbar = None  # Add reference for the toolbar
        self.ax = None
        self.line = None
        self.point_plot = None
        self.x_data = []
        self.y_data = []
        self.segment_colors = []
        self.delay_label_added = False
        self.G = None
        self.route_line = None
        self.detailed_route_line = None  # Track the detailed route line
        self.routes_plotted = False  # Track whether routes are currently plotted


    def setup_visualization(self, map_instance):
        """
        Sets up the visualization for traversal.
        """
        # Set up the figure and axis
        fig = plt.Figure(figsize=(8,8))
        self.ax = fig.add_subplot(111)
        self.ax.set_position([0, 0, 1, 1])  # left, bottom, right, top


        # Clear previous canvas and toolbar if any
        if self.canvas:
            self.canvas.get_tk_widget().destroy()
            self.canvas = None
        if self.toolbar:
            self.toolbar.destroy()
            self.toolbar = None

        # Create the canvas and add it to the Tkinter parent
        self.canvas = FigureCanvasTkAgg(fig, master=self.parent)
        self.canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

        # Add Matplotlib's built-in navigation toolbar
        self.toolbar = NavigationToolbar2Tk(self.canvas, self.parent)
        self.toolbar.update()
        self.toolbar.pack(side=tk.BOTTOM, fill=tk.X)

        # Customize toolbar button styles
        self.style_toolbar_buttons()

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
        self.current_index = 0
        self.elapsed_time = 0

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

    def style_toolbar_buttons(self):
        """
        Customizes the appearance of the toolbar buttons.
        """
        for button in self.toolbar.winfo_children():
            if isinstance(button, tk.Button):
                button.config(bg="darkgray", fg="black", activebackground="gray", bd=2)

    def toggle_routes(self, map_instance):
        """
        Toggles between plotting and clearing the routes (basic and detailed).
        """
        if not self.routes_plotted:
            self.plot_route(map_instance)
            self.plot_detailed_route(map_instance)
            self.routes_plotted = True
        else:
            self.clear_routes()
            self.routes_plotted = False

    def plot_route(self, map_instance):
        """
        Plots the basic route on the map.
        """
        print("Plotting route on the map...")
    
        route_latlng = [(map_instance.G.nodes[node]["y"], map_instance.G.nodes[node]["x"]) for node in map_instance.route]
        # Save the route line plot so it can be cleared later
        self.route_line, = self.ax.plot(
            [point[1] for point in route_latlng],
            [point[0] for point in route_latlng],
            linewidth=3, color="orange", label="Route"
        )
        self.canvas.draw()  # Force the canvas to update

    def plot_detailed_route(self, map_instance):
        """
        Plots the detailed route on the map with annotations.
        """
        print("Plotting detailed route on the map...")

        route_latlng = [(map_instance.G.nodes[node]["y"], map_instance.G.nodes[node]["x"]) for node in map_instance.route]
        # Save the detailed route line plot so it can be cleared later
        self.detailed_route_line, = self.ax.plot(
            [point[1] for point in route_latlng],
            [point[0] for point in route_latlng],
            linewidth=3, color="orange", label="Detailed Route"
        )

        # Annotate edges with distance, speed, and travel time
        for u, v in zip(map_instance.route[:-1], map_instance.route[1:]):
            edge_data = map_instance.G[u][v][0]
            mid_x = (map_instance.G.nodes[u]["x"] + map_instance.G.nodes[v]["x"]) / 2
            mid_y = (map_instance.G.nodes[u]["y"] + map_instance.G.nodes[v]["y"]) / 2

            distance = edge_data.get("length", 0)
            speed = edge_data.get("speed_kph", 0)
            travel_time = edge_data.get("travel_time", 0)

            annotation = f"{distance:.0f} m, {speed:.0f} km/h, {travel_time:.0f} s"
            self.ax.text(mid_x, mid_y, annotation, fontsize=8, color="blue", ha="center")

        self.canvas.draw()  # Force the canvas to update

    def clear_routes(self):
        """
        Clears both the basic route and the detailed route from the map.
        """
        print("Clearing routes from the map...")

        # Remove the basic route line
        if self.route_line is not None:
            if self.route_line in self.ax.lines:
                self.route_line.remove()
            self.route_line = None

        # Remove the detailed route line
        if self.detailed_route_line is not None:
            if self.detailed_route_line in self.ax.lines:
                self.detailed_route_line.remove()
            self.detailed_route_line = None

        # Clear text annotations manually
        texts_to_remove = list(self.ax.texts)  # Create a copy to avoid iteration issues
        for text in texts_to_remove:
            text.remove()

        # Redraw the canvas to reflect changes
        self.canvas.draw()
