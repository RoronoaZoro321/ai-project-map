# main.py

import tkinter as tk
from tkinter import messagebox
from configparser import ConfigParser
from tkinter import ttk  # Import ttk for styling

from gui.components import Components
from gui.visualization import Visualization
from logic.prolog_interface import PrologInterface
from logic.traversal import Traversal
from map.map import Map
from utils.utils import format_time  # Import the helper function

import os
import osmnx as ox  # Ensure osmnx is imported


def main():
    # Initialize Tkinter root
    root = tk.Tk()
    root.title("Shortest Path Finder")

    # Apply ttk theme
    style = ttk.Style()
    style.theme_use("clam")  # You can choose 'alt', 'default', 'classic', etc.

    # Initialize GUI components
    components = Components(root)
    visualization = Visualization(root)

    # Load configuration
    config = ConfigParser()
    config.read("config.ini")
    prolog_file_path = config.get("Prolog", "MainFile", fallback="./prolog/main.pl")

    # Initialize Prolog interface
    try:
        prolog_interface = PrologInterface(prolog_file_path)
    except Exception as e:
        messagebox.showerror(
            "Prolog Initialization Error", f"Failed to initialize Prolog: {e}"
        )
        return

    # Initialize map_instance as None
    root.map_instance = None  # Will be set after user inputs

    # Define event handlers
    def on_compute_button_click():
        """
        Event handler for the 'Compute Shortest Path' button click.
        """
        try:
            # Retrieve user inputs
            start_lat = float(components.start_lat_entry.get())
            start_lon = float(components.start_lon_entry.get())
            end_lat = float(components.end_lat_entry.get())
            end_lon = float(components.end_lon_entry.get())
            algorithm = components.algorithm_var.get()
            transportation_mode = components.transportation_mode_var.get()

            # Validate coordinates
            validate_coordinates(start_lat, start_lon, end_lat, end_lon)

            # Get speed based on transportation mode
            mode_speed = get_mode_speed(transportation_mode)

            # Create map and compute shortest path with the selected speed
            new_map_instance = Map(
                (start_lat, start_lon), (end_lat, end_lon), speed_kph=mode_speed
            )
            new_map_instance.prolog_interface = (
                prolog_interface  # Assign Prolog interface
            )
            prolog_interface.clear_dynamic_facts()
            prolog_interface.assert_nodes(new_map_instance.G)
            prolog_interface.assert_edges(new_map_instance.G)

            # Find nearest nodes
            orig_node = ox.distance.nearest_nodes(
                new_map_instance.G,
                X=new_map_instance.start_location[1],
                Y=new_map_instance.start_location[0],
            )
            dest_node = ox.distance.nearest_nodes(
                new_map_instance.G,
                X=new_map_instance.end_location[1],
                Y=new_map_instance.end_location[0],
            )
            new_map_instance.orig_node = orig_node
            new_map_instance.dest_node = dest_node
            new_map_instance.prolog_algorithm = algorithm

            # Compute shortest path via Prolog
            path, distance = prolog_interface.compute_shortest_path(
                orig_node, dest_node, algorithm
            )
            new_map_instance.route = path
            new_map_instance.total_distance = distance

            # Calculate traversal metrics
            calculate_traversal_metrics(new_map_instance)

            # Update the GUI components
            display_path(new_map_instance, components)
            visualization.setup_visualization(new_map_instance)

            # Store the map_instance
            root.map_instance = new_map_instance

            # Enable the 'Start Traversal' button
            components.start_traversal_button.config(state="normal")

        except Exception as e:
            messagebox.showerror("Error", str(e))

    def validate_coordinates(start_lat, start_lon, end_lat, end_lon):
        """
        Validates the latitude and longitude inputs.
        """
        for lat in [start_lat, end_lat]:
            if not (-90 <= lat <= 90):
                raise ValueError("Latitude must be between -90 and 90 degrees.")
        for lon in [start_lon, end_lon]:
            if not (-180 <= lon <= 180):
                raise ValueError("Longitude must be between -180 and 180 degrees.")

    def get_mode_speed(mode):
        """
        Retrieves the speed in km/h based on the selected transportation mode.

        Args:
            mode (str): Selected transportation mode.

        Returns:
            float: Speed in km/h.

        Raises:
            ValueError: If the mode is not recognized.
        """
        mode_speeds = {
            "car": 60.0,  # km/h
            "walking": 5.0,  # km/h
            "motorcycle": 40.0,  # km/h
        }
        if mode not in mode_speeds:
            raise ValueError(f"Unrecognized transportation mode: {mode}")
        return mode_speeds[mode]

    def calculate_traversal_metrics(map_instance):
        """
        Calculates distances, times, cumulative distances, and cumulative times for the path.
        """
        G = map_instance.G
        path = map_instance.route

        distances = []
        times = []
        cumulative_distances = [0]
        cumulative_times = [0]
        total_distance = 0
        total_time = 0

        for u, v in zip(path[:-1], path[1:]):
            # Handle potential missing edges due to delays
            if G.has_edge(u, v):
                data = G.get_edge_data(u, v)[0]
            elif G.has_edge(v, u):
                data = G.get_edge_data(v, u)[0]
            else:
                # If edge is missing, set default values
                data = {"length": 0, "travel_time": 0}
            length = data.get("length", 0)  # in meters
            travel_time = data.get("travel_time", 0)  # in seconds
            total_distance += length
            total_time += travel_time
            distances.append(length)
            times.append(travel_time)
            cumulative_distances.append(total_distance)
            cumulative_times.append(total_time)

        # Assign to map_instance
        map_instance.distances = distances
        map_instance.times = times
        map_instance.cumulative_distances = cumulative_distances
        map_instance.cumulative_times = cumulative_times
        map_instance.total_distance = total_distance
        map_instance.total_time = total_time

    def display_path(map_instance, components):
        """
        Displays the computed path in the GUI.
        """
        output = f"Shortest path from {map_instance.orig_node} to {map_instance.dest_node} using {map_instance.prolog_algorithm}:\n"
        output += f"Path (Node IDs):\n"
        for idx, node in enumerate(map_instance.route):
            output += f"{idx}: {node}\n"

        components.path_text.delete(1.0, tk.END)
        components.path_text.insert(tk.END, output)

        # Update total time and distance labels using format_time
        components.total_time_label.config(
            text=f"Total Time: {format_time(map_instance.total_time)}"
        )
        components.total_distance_label.config(
            text=f"Total Distance: {map_instance.total_distance:.2f} m"
        )

    def start_traversal():
        """
        Starts the traversal with random delays based on the specified probability.
        """
        try:
            map_instance = root.map_instance
            if not map_instance:
                messagebox.showerror("Error", "Compute the shortest path first.")
                return

            # Retrieve delay probability from user input
            delay_probability = (
                float(components.delay_prob_entry.get()) / 100.0
            )  # Convert percentage to decimal

            # Store the delay probability in map_instance
            map_instance.delay_probability = delay_probability

            # Set up traversal
            traversal = Traversal(map_instance, visualization, components, root)
            root.traversal = traversal  # Store traversal in root for access in cancel

            # Start the traversal
            traversal.start()

        except Exception as e:
            messagebox.showerror("Error", str(e))

    def cancel_traversal():
        """
        Cancels the traversal.
        """
        traversal = getattr(root, "traversal", None)
        if traversal:
            traversal.cancel()
            components.cancel_traversal_button.config(state="disabled")

    # Assign event handlers to buttons
    components.compute_button.config(command=on_compute_button_click)
    components.start_traversal_button.config(command=start_traversal)
    components.cancel_traversal_button.config(command=cancel_traversal)

    # Initialize attributes in root
    root.map_instance = None
    root.traversal = None

    root.mainloop()


def display_path(map_instance, components):
    """
    Displays the computed path in the GUI.
    """
    output = f"Shortest path from {map_instance.orig_node} to {map_instance.dest_node} using {map_instance.prolog_algorithm}:\n"
    output += f"Path (Node IDs):\n"
    for idx, node in enumerate(map_instance.route):
        output += f"{idx}: {node}\n"

    components.path_text.delete(1.0, tk.END)
    components.path_text.insert(tk.END, output)

    # Update total time and distance labels using format_time
    components.total_time_label.config(
        text=f"Total Time: {format_time(map_instance.total_time)}"
    )
    components.total_distance_label.config(
        text=f"Total Distance: {map_instance.total_distance:.2f} m"
    )


if __name__ == "__main__":
    main()
