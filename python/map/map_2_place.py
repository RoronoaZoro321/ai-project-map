import osmnx as ox
import matplotlib.pyplot as plt

def generate_map_of_malls(mall_a, mall_b):
    """
    Generates a map showing the locations of two shopping malls using OSMnx, with the mall locations highlighted.
    
    Parameters:
    mall_a (str): The name of the first shopping mall.
    mall_b (str): The name of the second shopping mall.
    """
    # Get the geographic locations of the two malls
    location_a = ox.geocode(mall_a)
    location_b = ox.geocode(mall_b)
    print("Location A:", location_a)
    print("Location B:", location_b)
    
    # Create a graph centered on the first mall location
    graph = ox.graph_from_point(location_a, dist=2000, network_type='drive')
    
    # Plot the map and highlight the mall locations
    fig, ax = plt.subplots(figsize=(8, 6))
    # Plot the graph on the ax
    ox.plot_graph(graph, ax=ax, node_color='skyblue', edge_color='gray', node_size=15, show=False, close=False)
    
    # Add markers for the mall locations (longitude, latitude)
    ax.plot(location_a[1], location_a[0], marker='o', markersize=15, color='red', label=mall_a)
    ax.plot(location_b[1], location_b[0], marker='o', markersize=15, color='blue', label=mall_b)
    
    # Add labels and title
    ax.set_title("Map of Two Shopping Malls")
    ax.legend()
    
    # Display the map
    plt.show()

generate_map_of_malls("Westfield San Francisco Centre", "The Grove Los Angeles")
