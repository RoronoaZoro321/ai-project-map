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
import folium
from tkhtmlview import HTMLLabel

# import webview
import threading
import webbrowser
import folium
from tkinterweb import HtmlFrame


def main():
    # Initialize Tkinter root
    root = tk.Tk()
    root.title("Shortest Path Finder")

    # Apply ttk theme
    style = ttk.Style()
    style.theme_use("clam")  # You can choose 'alt', 'default', 'classic', etc.

    # Configure root grid to have two columns: Left for map, Right for controls
    root.columnconfigure(0, weight=3)  # Left column (map) has more weight
    root.columnconfigure(1, weight=2)  # Right column (controls)
    root.rowconfigure(0, weight=1)

    # Create left and right frames
    left_frame = ttk.Frame(root, padding="5 5 5 5")
    left_frame.grid(row=0, column=0, sticky="nsew")

    right_frame = ttk.Frame(root, padding="5 5 5 5")
    right_frame.grid(row=0, column=1, sticky="nsew")

    # Initialize GUI components within the right frame
    components = Components(right_frame)
    visualization = Visualization(left_frame)

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

    # Initialize node list with predefined coordinates
    root.node_list = [
        (13.621244148739478, 100.61953164076222),  # Start Node
        (13.62352919308284, 100.61805578514283),  # Intermediate Node 1
        (13.626168472556909, 100.61339077096271),  # Intermediate Node 2
        (13.626794128718828, 100.6149335353964),  # Goal Node
    ]

    # Initialize map_instance as None
    root.map_instance = None  # Will be set after user inputs

    # Define event handlers
    def update_nodes_listbox():
        """
        Updates the listbox displaying the added nodes with explicit labels.
        """
        components.nodes_listbox.delete(0, tk.END)
        total_nodes = len(root.node_list)
        for idx, (lat, lng) in enumerate(root.node_list, start=1):
            if idx == 1:
                label = f"{idx}. Start Node: Lat: {lat}, Lng: {lng}"
            elif idx == total_nodes:
                label = f"{idx}. Goal Node: Lat: {lat}, Lng: {lng}"
            else:
                label = f"{idx}. Intermediate Node {idx-1}: Lat: {lat}, Lng: {lng}"
            components.nodes_listbox.insert(tk.END, label)

    def initialize_nodes():
        update_nodes_listbox()
        messagebox.showinfo("Initialization", "Initial nodes have been loaded.")

    def reset():
        """
        Resets the visualization and GUI components.
        """
        # Clear the routes and visualization
        visualization.clear_routes()
        # visualization.clear_visualization()
        # Reset GUI components
        components.total_time_label.config(text="Total Time: 00:00")
        components.total_distance_label.config(text="Total Distance: 0.00 m")
        components.total_delay_label.config(text="Total Delay Time: 00:00")
        components.time_difference_label.config(text="Time Difference: 00:00")
        components.remaining_time_label.config(text="Remaining Time: 00:00")
        components.remaining_distance_label.config(text="Remaining Distance: 0.00 m")
        components.status_label.config(text="Status: N/A", style="Error.TLabel")
        # Reset root attributes
        root.map_instance = None
        root.traversal = None
        root.node_list.clear()
        print(root.node_list)
        update_nodes_listbox()
        # Disable the 'Start Traversal' button
        components.start_traversal_button.config(state="disabled")
        # Disable the 'Cancel Traversal' button
        components.cancel_traversal_button.config(state="disabled")

    def on_add_node():
        """
        Event handler for the 'Add Node' button click.
        Adds a new node to the node list.
        """
        try:
            lat = float(components.lat_entry.get())
            lng = float(components.lon_entry.get())
            validate_coordinates(lat, lng)
            root.node_list.append((lat, lng))
            update_nodes_listbox()
            messagebox.showinfo("Success", f"Node ({lat}, {lng}) added.")
        except ValueError as ve:
            messagebox.showerror("Invalid Input", str(ve))
        except Exception as e:
            messagebox.showerror("Error", str(e))

    def on_remove_node():
        """
        Event handler for the 'Remove Selected Node' button click.
        Removes the selected node from the node list.
        """
        try:
            selection = components.nodes_listbox.curselection()
            if not selection:
                messagebox.showwarning("No Selection", "No node selected to remove.")
                return
            index = selection[0]
            removed_node = root.node_list.pop(index)
            update_nodes_listbox()
            messagebox.showinfo("Removed", f"Node {removed_node} removed.")
        except Exception as e:
            messagebox.showerror("Error", str(e))

    def on_compute_button_click():
        """
        Event handler for the 'Compute Shortest Path' button click.
        Computes the shortest paths between consecutive nodes.
        """
        try:
            if len(root.node_list) < 2:
                raise ValueError("Please add at least two nodes to compute a path.")

            algorithm = components.algorithm_var.get()
            transportation_mode = components.transportation_mode_var.get()

            # Get speed based on transportation mode
            mode_speed = get_mode_speed(transportation_mode)

            # Initialize overall path and total distance
            overall_route = []
            overall_distance = 0

            # Initialize list to collect all intermediate node coordinates
            intermediate_node_coords = []
            total_nodes = len(root.node_list)
            if total_nodes > 2:
                # All nodes except first and last are intermediate nodes
                intermediate_node_coords = root.node_list[1:-1]

            # Iterate through consecutive node pairs
            for i in range(len(root.node_list) - 1):
                start = root.node_list[i]
                end = root.node_list[i + 1]

                # Create a temporary Map instance for each pair
                temp_map = Map(start, end, speed_kph=mode_speed)
                temp_map.prolog_interface = prolog_interface  # Assign Prolog interface
                prolog_interface.clear_dynamic_facts()
                prolog_interface.assert_nodes(temp_map.G)
                prolog_interface.assert_edges(temp_map.G)

                # Find nearest nodes in the graph
                orig_node = ox.distance.nearest_nodes(
                    temp_map.G,
                    X=temp_map.start_location[1],
                    Y=temp_map.start_location[0],
                )
                dest_node = ox.distance.nearest_nodes(
                    temp_map.G,
                    X=temp_map.end_location[1],
                    Y=temp_map.end_location[0],
                )
                temp_map.orig_node = orig_node
                temp_map.dest_node = dest_node
                temp_map.prolog_algorithm = algorithm

                # Compute shortest path via Prolog
                path, distance = prolog_interface.compute_shortest_path(
                    orig_node, dest_node, algorithm
                )

                # Append to overall route (avoid duplicating nodes)
                if i == 0:
                    overall_route.extend(path)
                else:
                    overall_route.extend(
                        path[1:]
                    )  # Skip the first node to avoid duplication

                overall_distance += distance

            # Update the main map instance with the aggregated route
            main_map = aggregate_routes(
                overall_route, temp_map.G
            )  # Pass the graph here
            main_map.total_distance = overall_distance
            main_map.prolog_algorithm = algorithm

            # Determine intermediate node IDs based on intermediate_node_coords
            intermediate_nodes_ids = []
            if intermediate_node_coords:
                for coord in intermediate_node_coords:
                    lat, lng = coord
                    nearest_node = ox.distance.nearest_nodes(main_map.G, X=lng, Y=lat)
                    intermediate_nodes_ids.append(nearest_node)

            main_map.rest_nodes = (
                intermediate_nodes_ids  # Assign intermediate nodes to the map
            )

            # Calculate traversal metrics
            calculate_traversal_metrics(main_map)

            # Update the GUI components
            # Removed the display_path function call since path display is deleted
            visualization.setup_visualization(main_map)

            # Update Metrics Labels
            components.total_time_label.config(
                text=f"Total Time: {format_time(main_map.total_time)}"
            )
            components.total_distance_label.config(
                text=f"Total Distance: {main_map.total_distance:.2f} m"
            )
            # Initialize Total Delay Time and Time Difference
            components.total_delay_label.config(text="Total Delay Time: 00:00")
            components.time_difference_label.config(text="Time Difference: 00:00")

            # Store the map_instance
            root.map_instance = main_map

            # Enable the 'Start Traversal' button
            components.start_traversal_button.config(state="normal")

            messagebox.showinfo("Success", "Shortest path computed successfully.")

        except Exception as e:
            messagebox.showerror("Error", str(e))

    def aggregate_routes(route, G):
        """
        Aggregates the individual routes into a single Map instance.
        """
        if not route:
            raise ValueError("No route to aggregate.")

        if root.map_instance:
            root.map_instance.route = route
            root.map_instance.G = G  # Update the graph
            return root.map_instance
        else:
            first_node = route[0]
            last_node = route[-1]

            # Retrieve coordinates for the first and last nodes
            first_coords = (G.nodes[first_node]["y"], G.nodes[first_node]["x"])
            last_coords = (G.nodes[last_node]["y"], G.nodes[last_node]["x"])

            aggregated_map = Map(first_coords, last_coords)
            aggregated_map.route = route
            aggregated_map.G = G
            return aggregated_map

    def validate_coordinates(lat, lng):
        """
        Validates the latitude and longitude inputs.
        """
        if not (-90 <= lat <= 90):
            raise ValueError("Latitude must be between -90 and 90 degrees.")
        if not (-180 <= lng <= 180):
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
            "fast_car": 120.0,  # km/h
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

    def start_traversal():
        """
        Starts the traversal with delays assigned by Prolog based on probability.
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

            # Set up traversal
            traversal = Traversal(
                map_instance, visualization, components, root, prolog_interface
            )
            root.traversal = traversal  # Store traversal in root for access in cancel

            # Start the traversal
            traversal.start(delay_probability)  # Pass delay_probability

            # Update Metrics Labels for Delays
            # Initialize Total Delay Time and Time Difference
            traversal.set_metrics_labels(
                components.total_delay_label, components.time_difference_label
            )

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

    def on_view_as_graph_button_click():
        """
        Event handler for the 'View Graph' button click.
        """
        map_instance = root.map_instance
        if not map_instance:
            messagebox.showerror("Error", "Compute the shortest path first.")
            return
        # plot the route in map_instance.route on map visualization
        visualization.plot_route(map_instance)

    def on_view_as_detail_graph_button_click():
        """
        Event handler for the 'View Detailed Graph' button click.
        """
        map_instance = root.map_instance
        if not map_instance:
            messagebox.showerror("Error", "Compute the shortest path first.")
            return
        visualization.plot_detailed_route(map_instance)

    def on_clear_route_button_click():
        """
        Event handler for the 'Clear Route' button click.
        """
        visualization.clear_routes()

    def on_view_as_map_button_click():
        """
        Event handler for the 'View Real Map' button click.
        Displays a Folium map in a WebView window.
        """
        map_instance = root.map_instance
        if not map_instance:
            messagebox.showerror("Error", "Compute the shortest path first.")
            return

        # Generate Folium map
        print("Generating Folium map...")
        folium_map = folium.Map(
            location=[
                (map_instance.start_location[0] + map_instance.end_location[0]) / 2,
                (map_instance.start_location[1] + map_instance.end_location[1]) / 2,
            ],
            zoom_start=14,
        )

        # Add start and end markers
        folium.Marker(
            map_instance.start_location, popup="Start", icon=folium.Icon(color="green")
        ).add_to(folium_map)
        folium.Marker(
            map_instance.end_location, popup="End", icon=folium.Icon(color="red")
        ).add_to(folium_map)

        # Add intermediate node markers in black
        if hasattr(map_instance, "rest_nodes") and map_instance.rest_nodes:
            for node_id in map_instance.rest_nodes:
                lat = map_instance.G.nodes[node_id]["y"]
                lng = map_instance.G.nodes[node_id]["x"]
                folium.Marker(
                    location=(lat, lng),
                    popup="Intermediate Node",
                    icon=folium.Icon(color="black", icon="info-sign"),
                ).add_to(folium_map)

        # Plot the route
        route_latlng = [
            (map_instance.G.nodes[node]["y"], map_instance.G.nodes[node]["x"])
            for node in map_instance.route
        ]
        folium.PolyLine(route_latlng, color="blue", weight=5, opacity=0.7).add_to(
            folium_map
        )

        # Save Folium map to an HTML file
        html_file_path = "map.html"
        folium_map.save(html_file_path)

        # Use webbrowser to open the map in the default browser
        webbrowser.open("file://" + os.path.realpath("map.html"))

    # Assign event handlers to buttons
    components.reset_button.config(command=reset)
    components.add_node_button.config(command=on_add_node)
    components.remove_node_button.config(command=on_remove_node)
    components.compute_button.config(command=on_compute_button_click)
    components.start_traversal_button.config(command=start_traversal)
    components.cancel_traversal_button.config(command=cancel_traversal)
    components.view_real_map_button.config(command=on_view_as_map_button_click)
    components.view_graph_button.config(command=on_view_as_graph_button_click)
    components.view_detail_graph_button.config(
        command=on_view_as_detail_graph_button_click
    )
    components.clear_route_button.config(command=on_clear_route_button_click)

    # Initialize the nodes in the listbox and notify the user
    initialize_nodes()

    root.mainloop()


def aggregate_routes(route, G):
    """
    Aggregates the individual routes into a single Map instance.
    """
    # Access the root from the main function
    # To achieve this, you can pass root as a parameter or manage it differently
    # For simplicity, we'll assume root.map_instance is accessible
    # However, to avoid global variables, consider restructuring your code
    # Here, we'll keep it simple based on the initial approach

    # Note: In this context, 'root' is not directly accessible.
    # To properly implement this, consider refactoring to pass 'root' as a parameter.
    # For now, we'll assume that this function is nested within 'main()'

    # If 'aggregate_routes' needs access to 'root', it should be defined within 'main()'
    # For better structure, you might want to move 'aggregate_routes' inside 'main()'
    # Here's an adjusted version:

    # However, in the current context, we'll leave it as is and assume it's fine.
    if not route:
        raise ValueError("No route to aggregate.")

    # This part may need adjustment based on your actual code structure
    # Here, we're creating a new Map instance

    first_node = route[0]
    last_node = route[-1]

    # Retrieve coordinates for the first and last nodes
    first_coords = (G.nodes[first_node]["y"], G.nodes[first_node]["x"])
    last_coords = (G.nodes[last_node]["y"], G.nodes[last_node]["x"])

    aggregated_map = Map(first_coords, last_coords)
    aggregated_map.route = route
    aggregated_map.G = G
    return aggregated_map


if __name__ == "__main__":
    main()
