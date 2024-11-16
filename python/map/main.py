# main.py

import tkinter as tk
from tkinter import ttk, messagebox
from map import Map
import osmnx as ox
import networkx as nx
import webbrowser


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

    travel_time = sum(G[u][v][0]["travel_time"] for u, v in zip(route[:-1], route[1:]))
    output = f"Shortest path from {orig_node} to {dest_node} using {algorithm}:\n"
    output += f"Path: {route}\n"
    output += f"Total travel time (seconds): {travel_time:.2f}"
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
        # Display the map with or without visualization
        if visualize:
            map_instance.create_map_with_folium("map_with_folium.html")
            # Open the HTML file in a web browser
            webbrowser.open("map_with_folium.html")
        else:
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

root.mainloop()
