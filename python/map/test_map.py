import osmnx as ox
import matplotlib.pyplot as plt

# Define a smaller town for faster rendering
town_name = "Bang Mueang Mai, Thailand"

# Download the street network for the specified town
graph = ox.graph_from_place(town_name, network_type="all") # “all_private”, “all”, “bike”, “drive”, “drive_service”, “walk”

# Plot the map with color customization
fig, ax = ox.plot_graph(
    graph,
    node_size=10,
    node_color="skyblue",      # Light blue nodes
    edge_linewidth=1,
    edge_color="coral",        # Coral-colored edges
    bgcolor="lightgray"        # Light gray background
)

plt.show()
