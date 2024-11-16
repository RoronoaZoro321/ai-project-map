# main.py

import tkinter as tk
from tkinter import ttk, messagebox
from map import Map
import osmnx as ox
import networkx as nx
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
import time
import random
from pyswip import Prolog  # Import PySwip for Prolog interaction


def create_map(start_lat, start_lon, end_lat, end_lon):
    """
    Create a Map instance with user-provided locations.
    """
    start_location = (start_lat, start_lon)
    end_location = (end_lat, end_lon)
    map_instance = Map(start_location, end_location)
    return map_instance


def compute_shortest_path(map_instance, algorithm):
    """
    Compute the shortest path using Prolog and update the map instance with route information.
    """
    G = map_instance.G

    # Set the start and end nodes to the nearest nodes in the map
    orig_node = ox.distance.nearest_nodes(
        G, X=map_instance.start_location[1], Y=map_instance.start_location[0]
    )
    dest_node = ox.distance.nearest_nodes(
        G, X=map_instance.end_location[1], Y=map_instance.end_location[0]
    )

    # Set up Prolog environment
    prolog = Prolog()
    prolog.consult("../../prolog/main.pl")

    # Clear previous dynamic facts
    list(prolog.query("retractall(node(_, _, _))"))
    list(prolog.query("retractall(edge(_, _, _))"))
    list(prolog.query("retractall(delayed_edge(_, _))"))

    # Assert nodes into Prolog
    for node_id, data in G.nodes(data=True):
        lat = data["y"]
        lon = data["x"]
        prolog.assertz(f"node({node_id}, {lat}, {lon})")

    # Assert edges into Prolog
    for u, v, data in G.edges(data=True):
        weight = data["length"]
        prolog.assertz(f"edge({u}, {v}, {weight})")
        # Assuming undirected graph
        prolog.assertz(f"edge({v}, {u}, {weight})")

    # Map variables to be accessible globally
    map_instance.orig_node = orig_node
    map_instance.dest_node = dest_node
    map_instance.prolog = prolog
    map_instance.G = G

    # Call Prolog to compute shortest path
    if algorithm == "dijkstra":
        prolog_algorithm = "dijkstra"
    elif algorithm == "astar":
        prolog_algorithm = "astar"
    else:
        raise ValueError("Unsupported algorithm")

    map_instance.prolog_algorithm = prolog_algorithm  # Store algorithm

    query = (
        f"shortest_path({orig_node}, {dest_node}, {prolog_algorithm}, Path, Distance)"
    )
    result = list(prolog.query(query))

    if result:
        path = result[0]["Path"]
        distance = result[0]["Distance"]
        # Convert Prolog list to Python list
        path = list(path)

        map_instance.route = path  # Update the route in the map instance

        # Compute distances and times
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
            length = data["length"]  # in meters
            travel_time = data["travel_time"]  # in seconds
            total_distance += length
            total_time += travel_time
            distances.append(length)
            times.append(travel_time)
            cumulative_distances.append(total_distance)
            cumulative_times.append(total_time)

        map_instance.distances = distances
        map_instance.times = times
        map_instance.cumulative_distances = cumulative_distances
        map_instance.cumulative_times = cumulative_times
        map_instance.total_distance = total_distance
        map_instance.total_time = total_time

        # Display the path with indices in the GUI
        output = f"Shortest path from {orig_node} to {dest_node} using {algorithm}:\n"
        output += f"Path (Node IDs):\n"
        for idx, node in enumerate(path):
            output += f"{idx}: {node}\n"

        path_text.delete(1.0, tk.END)
        path_text.insert(tk.END, output)

        # Update total time and distance labels
        total_time_label.config(text=f"Total Time: {map_instance.total_time:.2f} sec")
        total_distance_label.config(
            text=f"Total Distance: {map_instance.total_distance:.2f} m"
        )

        # Store map_instance for use in traversal
        root.map_instance = map_instance

        return
    else:
        raise Exception("No path found")


def on_compute_button_click():
    """
    Event handler for the 'Compute Shortest Path' button click.
    """
    try:
        # Retrieve user inputs
        start_lat = float(start_lat_entry.get())
        start_lon = float(start_lon_entry.get())
        end_lat = float(end_lat_entry.get())
        end_lon = float(end_lon_entry.get())
        algorithm = algorithm_var.get()

        # Create map and compute shortest path
        map_instance = create_map(start_lat, start_lon, end_lat, end_lon)
        compute_shortest_path(map_instance, algorithm)

        # Enable the 'Start Traversal' button
        start_traversal_button.config(state="normal")
    except Exception as e:
        messagebox.showerror("Error", str(e))


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
            float(delay_prob_entry.get()) / 100.0
        )  # Convert percentage to decimal

        # Store the delay probability in map_instance
        map_instance.delay_probability = delay_probability

        # Set up visualization and traversal
        setup_visualization(map_instance)

        # Start the traversal
        root.cancel_traversal = False  # Reset the cancel flag
        root.update_traversal()
    except Exception as e:
        messagebox.showerror("Error", str(e))


def setup_visualization(map_instance):
    """
    Sets up the visualization for traversal.
    """
    # Set up the figure and canvas
    fig = plt.Figure(figsize=(8, 8))
    ax = fig.add_subplot(111)

    # Clear previous canvas if any
    if hasattr(root, "canvas") and root.canvas is not None:
        root.canvas.get_tk_widget().destroy()
        root.canvas = None

    root.canvas = FigureCanvasTkAgg(fig, master=root)
    root.canvas.get_tk_widget().grid(row=12, column=0, columnspan=2)

    # Plot the graph
    ox.plot_graph(
        map_instance.G,
        ax=ax,
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
    ax.plot(
        map_instance.start_location[1],
        map_instance.start_location[0],
        marker="o",
        color="green",
        markersize=10,
        label="Start",
    )
    ax.plot(
        map_instance.end_location[1],
        map_instance.end_location[0],
        marker="o",
        color="red",
        markersize=10,
        label="End",
    )

    # Initialize the line and point plots
    (line,) = ax.plot([], [], color="purple", linewidth=3, label="Route")
    (point_plot,) = ax.plot([], [], marker="o", color="blue", markersize=8)

    # Store necessary variables in root
    root.map_instance = map_instance
    root.start_time = time.time()
    root.line = line
    root.point_plot = point_plot
    root.ax = ax  # Store the axes for updating lines

    # Prepare data for animation
    x_data = [point[1] for point in route_latlng]
    y_data = [point[0] for point in route_latlng]
    root.x_data = x_data
    root.y_data = y_data

    # Variables to track traversal progress
    root.current_index = 0  # Index in the route
    root.elapsed_time = 0  # Total elapsed time

    # Variables to store colors for each segment
    root.segment_colors = ["purple"] * (len(map_instance.route) - 1)

    ax.set_title("Real-Time Route Traversal")
    ax.legend()
    root.canvas.draw()

    # Save original edges in case we need to restore them
    map_instance.original_edges = {}
    for u, v, data in map_instance.G.edges(data=True):
        map_instance.original_edges[(u, v)] = data

    # Initialize status message
    status_label.config(text="Traversal started.")

    # Enable 'Cancel Traversal' button
    cancel_traversal_button.config(state="normal")


def update_traversal():
    """
    Updates the traversal visualization and logic.
    """
    if root.cancel_traversal:
        status_label.config(text="Traversal cancelled.")
        return

    # Time since last update
    time_now = time.time()
    delta_time = time_now - root.start_time
    root.start_time = time_now
    map_instance = root.map_instance
    x_data = root.x_data
    y_data = root.y_data
    line = root.line
    point_plot = root.point_plot
    ax = root.ax

    # Update elapsed time
    root.elapsed_time += delta_time

    cumulative_times = map_instance.cumulative_times
    idx = root.current_index

    # Check if traversal is complete
    if idx >= len(cumulative_times) - 1:
        # Animation finished
        remaining_time_label.config(text=f"Remaining Time: 0.00 sec")
        remaining_distance_label.config(text=f"Remaining Distance: 0.00 m")
        # Set marker to end position
        point_plot.set_data(x_data[-1], y_data[-1])
        # Draw all segments with their assigned colors
        for i in range(len(x_data) - 1):
            ax.plot(
                [x_data[i], x_data[i + 1]],
                [y_data[i], y_data[i + 1]],
                color=root.segment_colors[i],
                linewidth=3,
            )
        root.canvas.draw()
        status_label.config(text="Traversal completed.")
        return

    # Update remaining time and distance
    total_time = map_instance.total_time
    elapsed_time = root.elapsed_time
    remaining_time = total_time - elapsed_time
    remaining_distance = map_instance.total_distance * (remaining_time / total_time)
    remaining_time_label.config(
        text=f"Remaining Time: {max(remaining_time, 0):.2f} sec"
    )
    remaining_distance_label.config(
        text=f"Remaining Distance: {max(remaining_distance, 0):.2f} m"
    )

    # Get current and next nodes
    current_node = map_instance.route[idx]
    next_node = map_instance.route[idx + 1]

    # Update current segment
    x_prev = x_data[idx]
    y_prev = y_data[idx]
    x_curr = x_data[idx + 1]
    y_curr = y_data[idx + 1]

    time_prev = cumulative_times[idx]
    time_curr = cumulative_times[idx + 1]
    frac = (
        (root.elapsed_time - time_prev) / (time_curr - time_prev)
        if (time_curr - time_prev) != 0
        else 0
    )

    # Interpolate position
    x = x_prev + frac * (x_curr - x_prev)
    y = y_prev + frac * (y_curr - y_prev)

    # Update marker position
    point_plot.set_data(x, y)

    # Initialize the path color
    path_color = root.segment_colors[idx]  # Use the assigned color

    # Randomly decide if a delay occurs on the current edge
    delay_probability = map_instance.delay_probability
    delay_occurred = random.random() < delay_probability

    if delay_occurred and root.segment_colors[idx] == "purple":
        # Delay occurs on this segment for the first time
        # Inform Prolog about the delay
        map_instance.prolog.assertz(f"delayed_edge({current_node}, {next_node})")
        map_instance.prolog.assertz(f"delayed_edge({next_node}, {current_node})")

        # Remove the edge from NetworkX graph (for visualization)
        if map_instance.G.has_edge(current_node, next_node):
            map_instance.G.remove_edge(current_node, next_node)
        if map_instance.G.has_edge(next_node, current_node):
            map_instance.G.remove_edge(next_node, current_node)

        # Recompute the shortest path from current node to goal
        new_start_node = current_node  # Our current node
        query = f"shortest_path({new_start_node}, {map_instance.dest_node}, {map_instance.prolog_algorithm}, Path, Distance)"
        result = list(map_instance.prolog.query(query))

        if result:
            # Path changed due to delay
            new_path = result[0]["Path"]
            new_distance = result[0]["Distance"]
            new_path = list(new_path)
            # Update the route: keep the path up to current node and append new path
            old_route_length = len(map_instance.route)
            map_instance.route = map_instance.route[: idx + 1] + new_path[1:]

            # Update x_data, y_data
            route_latlng = [
                (map_instance.G.nodes[node]["y"], map_instance.G.nodes[node]["x"])
                for node in map_instance.route
            ]
            root.x_data = x_data = [point[1] for point in route_latlng]
            root.y_data = y_data = [point[0] for point in route_latlng]

            # Update cumulative_times and cumulative_distances
            # Recalculate from current index onwards
            map_instance.distances = map_instance.distances[:idx]
            map_instance.times = map_instance.times[:idx]
            map_instance.cumulative_distances = map_instance.cumulative_distances[
                : idx + 1
            ]
            map_instance.cumulative_times = map_instance.cumulative_times[: idx + 1]
            map_instance.total_distance = map_instance.cumulative_distances[-1]
            map_instance.total_time = map_instance.cumulative_times[-1]

            for u, v in zip(map_instance.route[idx:], map_instance.route[idx + 1 :]):
                # Handle potential missing edges
                if map_instance.G.has_edge(u, v):
                    data = map_instance.G.get_edge_data(u, v)[0]
                elif map_instance.G.has_edge(v, u):
                    data = map_instance.G.get_edge_data(v, u)[0]
                else:
                    # If edge is missing, set default values
                    data = {"length": 0, "travel_time": 0}

                length = data["length"]  # in meters
                travel_time = data["travel_time"]  # in seconds
                map_instance.total_distance += length
                map_instance.total_time += travel_time
                map_instance.distances.append(length)
                map_instance.times.append(travel_time)
                map_instance.cumulative_distances.append(map_instance.total_distance)
                map_instance.cumulative_times.append(map_instance.total_time)

            # Update total time and distance labels
            total_time_label.config(
                text=f"Total Time: {map_instance.total_time:.2f} sec"
            )
            total_distance_label.config(
                text=f"Total Distance: {map_instance.total_distance:.2f} m"
            )

            # Update segment colors:
            # - Current segment (where delay occurred): 'red'
            # - Next segment (first edge of new path): 'red'
            # - Remaining segments: 'purple'

            new_segment_count = len(map_instance.route) - idx - 1
            # Only update colors up to the next segment
            root.segment_colors[idx] = "red"  # Current delayed edge
            if new_segment_count >= 1:
                root.segment_colors = (
                    root.segment_colors[: idx + 1]
                    + ["red"]
                    + ["purple"] * (new_segment_count - 1)
                )
            else:
                root.segment_colors = root.segment_colors[: idx + 1]

            path_color = "red"
            status_label.config(
                text=f"Delay between Node {current_node} and Node {next_node}. Path changed."
            )

        else:
            # No alternative path found
            # Remove the delayed_edge assertions
            map_instance.prolog.retractall(f"delayed_edge({current_node}, {next_node})")
            map_instance.prolog.retractall(f"delayed_edge({next_node}, {current_node})")
            # Add the edge back to the NetworkX graph
            original_data = map_instance.original_edges.get((current_node, next_node))
            if original_data:
                map_instance.G.add_edge(current_node, next_node, **original_data)
            # Continue on the original path
            path_color = "yellow"
            # Update segment color for current segment to 'yellow'
            root.segment_colors[idx] = "yellow"
            status_label.config(
                text=f"Delay between Node {current_node} and Node {next_node}, but no alternative path. Continuing."
            )

    else:
        # Normal traversal or delay already processed
        path_color = root.segment_colors[idx]
        status_label.config(
            text=f"Traversing edge {idx}: Node {current_node} -> Node {next_node}."
        )

    # Update line to show current segment with appropriate color
    line.set_data([x_prev, x_curr], [y_prev, y_curr])
    line.set_color(path_color)
    root.canvas.draw()

    # Move to the next segment if time allows
    if root.elapsed_time >= cumulative_times[idx + 1]:
        # Draw the segment permanently
        ax.plot([x_prev, x_curr], [y_prev, y_curr], color=path_color, linewidth=3)
        root.current_index += 1

    # Schedule next update in 0.1 second
    root.after(100, root.update_traversal)


def cancel_traversal():
    """
    Cancels the traversal.
    """
    root.cancel_traversal = True
    cancel_traversal_button.config(state="disabled")
    status_label.config(text="Traversal cancelled.")


# Set up the Tkinter GUI
root = tk.Tk()
root.title("Shortest Path Finder")

# Algorithm selection
algorithm_var = tk.StringVar(value="dijkstra")
algorithm_label = ttk.Label(root, text="Select Algorithm:")
algorithm_label.grid(row=0, column=0, padx=5, pady=5, sticky="w")
algorithm_dropdown = ttk.OptionMenu(
    root, algorithm_var, "dijkstra", "dijkstra", "astar"
)
algorithm_dropdown.grid(row=0, column=1, padx=5, pady=5, sticky="w")

# Start location entries
start_lat_label = ttk.Label(root, text="Start Latitude:")
start_lat_label.grid(row=1, column=0, padx=5, pady=5, sticky="w")
start_lat_entry = ttk.Entry(root)
start_lat_entry.grid(row=1, column=1, padx=5, pady=5, sticky="w")
start_lat_entry.insert(0, "13.621244148739478")  # Default value

start_lon_label = ttk.Label(root, text="Start Longitude:")
start_lon_label.grid(row=2, column=0, padx=5, pady=5, sticky="w")
start_lon_entry = ttk.Entry(root)
start_lon_entry.grid(row=2, column=1, padx=5, pady=5, sticky="w")
start_lon_entry.insert(0, "100.61953164076222")  # Default value

# End location entries
end_lat_label = ttk.Label(root, text="End Latitude:")
end_lat_label.grid(row=3, column=0, padx=5, pady=5, sticky="w")
end_lat_entry = ttk.Entry(root)
end_lat_entry.grid(row=3, column=1, padx=5, pady=5, sticky="w")
end_lat_entry.insert(0, "13.626794128718828")  # Default value

end_lon_label = ttk.Label(root, text="End Longitude:")
end_lon_label.grid(row=4, column=0, padx=5, pady=5, sticky="w")
end_lon_entry = ttk.Entry(root)
end_lon_entry.grid(row=4, column=1, padx=5, pady=5, sticky="w")
end_lon_entry.insert(0, "100.6149335353964")  # Default value

# Compute button
compute_button = ttk.Button(
    root, text="Compute Shortest Path", command=on_compute_button_click
)
compute_button.grid(row=5, column=0, columnspan=2, padx=5, pady=10)

# Path display
path_label = ttk.Label(root, text="Computed Path:")
path_label.grid(row=6, column=0, padx=5, pady=5, sticky="w")
path_text = tk.Text(root, height=10, width=50)
path_text.grid(row=7, column=0, columnspan=2, padx=5, pady=5)

# Delay probability input
delay_prob_label = ttk.Label(root, text="Delay Probability (%):")
delay_prob_label.grid(row=8, column=0, padx=5, pady=5, sticky="w")
delay_prob_entry = ttk.Entry(root)
delay_prob_entry.grid(row=8, column=1, padx=5, pady=5, sticky="w")
delay_prob_entry.insert(0, "10")  # Default to 10%

# Start traversal button
start_traversal_button = ttk.Button(
    root, text="Start Traversal", command=start_traversal, state="disabled"
)
start_traversal_button.grid(row=9, column=0, padx=5, pady=10)

# Cancel traversal button
cancel_traversal_button = ttk.Button(
    root, text="Cancel Traversal", command=cancel_traversal, state="disabled"
)
cancel_traversal_button.grid(row=9, column=1, padx=5, pady=10)

# Total time label
total_time_label = ttk.Label(root, text="Total Time: N/A")
total_time_label.grid(row=10, column=0, padx=5, pady=5, sticky="w")

# Total distance label
total_distance_label = ttk.Label(root, text="Total Distance: N/A")
total_distance_label.grid(row=11, column=0, padx=5, pady=5, sticky="w")

# Remaining time label
remaining_time_label = ttk.Label(root, text="Remaining Time: N/A")
remaining_time_label.grid(row=10, column=1, padx=5, pady=5, sticky="w")

# Remaining distance label
remaining_distance_label = ttk.Label(root, text="Remaining Distance: N/A")
remaining_distance_label.grid(row=11, column=1, padx=5, pady=5, sticky="w")

# Status message label
status_label = ttk.Label(root, text="Status: N/A")
status_label.grid(row=13, column=0, columnspan=2, padx=5, pady=5, sticky="w")

root.canvas = None
root.map_instance = None
root.cancel_traversal = False

root.update_traversal = update_traversal  # Attach the function to root

root.mainloop()
