# main.py

import tkinter as tk
from tkinter import ttk, messagebox
from map import Map
import osmnx as ox
import networkx as nx
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
import time


def create_map(start_lat, start_lon, end_lat, end_lon):
    # Create Map instance with user-provided locations
    start_location = (start_lat, start_lon)
    end_location = (end_lat, end_lon)
    map_instance = Map(start_location, end_location)
    return map_instance


def compute_shortest_path(map_instance, algorithm):
    G = map_instance.G
    orig_node = ox.distance.nearest_nodes(
        G, X=map_instance.start_location[1], Y=map_instance.start_location[0]
    )
    dest_node = ox.distance.nearest_nodes(
        G, X=map_instance.end_location[1], Y=map_instance.end_location[0]
    )

    if algorithm == "dijkstra":
        route = nx.shortest_path(
            G, orig_node, dest_node, weight="travel_time", method="dijkstra"
        )
    elif algorithm == "astar":
        # Use the heuristic function from networkx
        heuristic = lambda u, v: ox.distance.euclidean_dist_vec(
            G.nodes[u]["y"], G.nodes[u]["x"], G.nodes[v]["y"], G.nodes[v]["x"]
        )
        route = nx.astar_path(
            G, orig_node, dest_node, heuristic=heuristic, weight="travel_time"
        )
    else:
        raise ValueError("Unsupported algorithm")

    map_instance.route = route  # Update the route in the map instance

    # Compute distances and times
    distances = []
    times = []
    cumulative_distances = [0]
    cumulative_times = [0]
    total_distance = 0
    total_time = 0

    for u, v in zip(route[:-1], route[1:]):
        data = G.get_edge_data(u, v)[0]
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

    output = f"Shortest path from {orig_node} to {dest_node} using {algorithm}:\n"
    output += f"Path: {route}\n"
    output += f"Total travel time (seconds): {total_time:.2f}\n"
    output += f"Total distance (meters): {total_distance:.2f}"
    return output


def on_compute_button_click():
    try:
        # Retrieve user inputs
        start_lat = float(start_lat_entry.get())
        start_lon = float(start_lon_entry.get())
        end_lat = float(end_lat_entry.get())
        end_lon = float(end_lon_entry.get())
        algorithm = algorithm_var.get()
        visualize = visualize_var.get()

        # Create map and compute shortest path
        map_instance = create_map(start_lat, start_lon, end_lat, end_lon)
        result = compute_shortest_path(map_instance, algorithm)
        # Display the result in a message box
        messagebox.showinfo("Shortest Path Result", result)

        # Update the labels for total time and distance
        total_time_label.config(text=f"Total Time: {map_instance.total_time:.2f} sec")
        total_distance_label.config(
            text=f"Total Distance: {map_instance.total_distance:.2f} m"
        )
        remaining_time_label.config(
            text=f"Remaining Time: {map_instance.total_time:.2f} sec"
        )
        remaining_distance_label.config(
            text=f"Remaining Distance: {map_instance.total_distance:.2f} m"
        )

        if visualize:
            # Set up the figure and canvas
            fig = plt.Figure(figsize=(8, 8))
            ax = fig.add_subplot(111)

            # Clear previous canvas if any
            if hasattr(root, "canvas") and root.canvas is not None:
                root.canvas.get_tk_widget().destroy()
                root.canvas = None

            root.canvas = FigureCanvasTkAgg(fig, master=root)
            root.canvas.get_tk_widget().grid(row=9, column=0, columnspan=2)

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

            # Prepare data for animation
            x_data = [point[1] for point in route_latlng]
            y_data = [point[0] for point in route_latlng]

            (line,) = ax.plot([], [], color="purple", linewidth=3, label="Route")
            (point_plot,) = ax.plot([], [], marker="o", color="blue", markersize=8)

            # Store necessary variables in root
            root.map_instance = map_instance
            root.start_time = time.time()
            root.line = line
            root.point_plot = point_plot
            root.x_data = x_data
            root.y_data = y_data

            ax.set_title("Real-Time Route Traversal")
            ax.legend()
            root.canvas.draw()

            # Define the update_traversal function and attach it to root
            def update_traversal():
                elapsed_time = time.time() - root.start_time
                map_instance = root.map_instance
                x_data = root.x_data
                y_data = root.y_data
                line = root.line
                point_plot = root.point_plot

                if elapsed_time >= map_instance.total_time:
                    # Animation finished
                    remaining_time_label.config(text=f"Remaining Time: 0.00 sec")
                    remaining_distance_label.config(text=f"Remaining Distance: 0.00 m")
                    # Set marker to end position
                    point_plot.set_data(x_data[-1], y_data[-1])
                    line.set_data(x_data, y_data)
                    root.canvas.draw()
                    return
                else:
                    # Update remaining time and distance
                    remaining_time = map_instance.total_time - elapsed_time
                    remaining_distance = map_instance.total_distance * (
                        remaining_time / map_instance.total_time
                    )
                    remaining_time_label.config(
                        text=f"Remaining Time: {remaining_time:.2f} sec"
                    )
                    remaining_distance_label.config(
                        text=f"Remaining Distance: {remaining_distance:.2f} m"
                    )

                    # Compute current position along the route
                    cumulative_times = map_instance.cumulative_times
                    idx = next(
                        (i for i, t in enumerate(cumulative_times) if t > elapsed_time),
                        len(cumulative_times) - 1,
                    )

                    if idx == 0:
                        time_prev = 0
                        time_curr = cumulative_times[1]
                        frac = elapsed_time / time_curr if time_curr != 0 else 0
                        x_prev = x_data[0]
                        x_curr = x_data[1]
                        y_prev = y_data[0]
                        y_curr = y_data[1]
                    else:
                        time_prev = cumulative_times[idx - 1]
                        time_curr = cumulative_times[idx]
                        frac = (
                            (elapsed_time - time_prev) / (time_curr - time_prev)
                            if (time_curr - time_prev) != 0
                            else 0
                        )
                        x_prev = x_data[idx - 1]
                        x_curr = x_data[idx]
                        y_prev = y_data[idx - 1]
                        y_curr = y_data[idx]

                    # Interpolate position
                    x = x_prev + frac * (x_curr - x_prev)
                    y = y_prev + frac * (y_curr - y_prev)

                    # Update marker position
                    point_plot.set_data(x, y)

                    # Update line
                    line.set_data(x_data[: idx + 1], y_data[: idx + 1])
                    root.canvas.draw()

                    # Schedule next update in 1 second
                    root.after(1000, root.update_traversal)

            # Attach the function to root
            root.update_traversal = update_traversal

            # Start the animation
            root.update_traversal()
        else:
            # Display static map without traversal
            map_instance.create_map_without_folium()
    except Exception as e:
        messagebox.showerror("Error", str(e))


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

# Visualization option
visualize_var = tk.BooleanVar(value=True)
visualize_checkbox = ttk.Checkbutton(
    root, text="Simulate Real-Time Traversal", variable=visualize_var
)
visualize_checkbox.grid(row=5, column=0, columnspan=2, padx=5, pady=5, sticky="w")

# Compute button
compute_button = ttk.Button(
    root, text="Compute Shortest Path", command=on_compute_button_click
)
compute_button.grid(row=6, column=0, columnspan=2, padx=5, pady=10)

# Total time label
total_time_label = ttk.Label(root, text="Total Time: N/A")
total_time_label.grid(row=7, column=0, padx=5, pady=5, sticky="w")

# Total distance label
total_distance_label = ttk.Label(root, text="Total Distance: N/A")
total_distance_label.grid(row=8, column=0, padx=5, pady=5, sticky="w")

# Remaining time label
remaining_time_label = ttk.Label(root, text="Remaining Time: N/A")
remaining_time_label.grid(row=7, column=1, padx=5, pady=5, sticky="w")

# Remaining distance label
remaining_distance_label = ttk.Label(root, text="Remaining Distance: N/A")
remaining_distance_label.grid(row=8, column=1, padx=5, pady=5, sticky="w")

root.canvas = None

root.mainloop()
from geocoding import Geocoder

def create_map_with_geocoding():
    # Usage
    start_address = "Soi Mu Ban Prem Ruethai 11"
    end_address = "Sridan 3/3 Alley"
    # Create Geocoder instance
    geocoder = Geocoder()
    # Geocode the start and end addresses
    start_location = geocoder.geocode(start_address)
    end_location = geocoder.geocode(end_address)
    print("start_location:", start_location)
    print("end_location:", end_location)
    # Create Map instance
    map = Map(start_location, end_location)
    return map

def create_map():
    # Usage
    start_location = (13.621244148739478, 100.61953164076222)  # San Francisco
    end_location = (13.626794128718828, 100.6149335353964)  # Near San Francisco
    # Create Map instance
    map = Map(start_location, end_location)
    return map

def display_map(map):
    # Create a map with folium and save it as an HTML file
    map.create_map_with_folium("map_with_folium.html")
    # Create a static map without folium
    map.create_map_without_folium()

def get_edge(map):
    # Retrieve all edges and assert them as facts in Prolog
    edges = map.get_all_edges()
    print("Edges:", edges)

    # Check facts by querying Prolog
    edge_query_result = list(map.prolog.query("edge(X, Y)"))
    print("Edges:", edge_query_result)

def get_node(map):
    # Retrieve all nodes and assert them as facts in Prolog
    nodes = map.get_all_nodes()
    print("Nodes:", nodes)

    # Check facts by querying Prolog
    node_query_result = list(map.prolog.query("node(X, Lat, Lon)"))
    print("Nodes:", node_query_result)

def get_weight(map):
    # Retrieve all weights and assert them as facts in Prolog
    weights = map.get_all_weights()
    print("Weights:", weights)

    # Check facts by querying Prolog
    weight_query_result = list(map.prolog.query("weight(X, Y, TravelTime)"))
    print("Weights:", weight_query_result)


def get_fact(map):
    # Retrieve all edges, nodes, and weights and assert them as facts in Prolog
    get_edge(map)
    get_node(map)
    get_weight(map)
def main():
    # map = create_map_with_geocoding()
    map = create_map()
    print(map)
    display_map(map)
    get_fact(map)

if __name__ == "__main__":
    main()


