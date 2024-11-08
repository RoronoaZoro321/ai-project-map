import folium
import osmnx as ox
from geopy.distance import geodesic

def create_location_map(start_location, end_location):
    """
    Create a folium map showing the road route between two locations.
    
    Parameters:
    start_location (tuple): Latitude and longitude of the starting location
    end_location (tuple): Latitude and longitude of the ending location
    """
    # Determine bounding box based on distance between points
    distance_km = geodesic(start_location, end_location).km
    buffer_dist = distance_km * 500  # Adjust based on your needs for a larger area

    # Define bounding box around midpoint between start and end locations
    midpoint = ((start_location[0] + end_location[0]) / 2, (start_location[1] + end_location[1]) / 2)
    G = ox.graph_from_point(midpoint, dist=buffer_dist, network_type='drive')
    G = ox.add_edge_speeds(G)
    G = ox.add_edge_travel_times(G)

    # Find the nearest nodes in the graph to the start and end locations
    orig_node = ox.distance.nearest_nodes(G, X=start_location[1], Y=start_location[0])
    dest_node = ox.distance.nearest_nodes(G, X=end_location[1], Y=end_location[0])
    
    # Get the shortest path by travel time
    route = ox.shortest_path(G, orig_node, dest_node, weight='travel_time')
    
    # Create the folium map
    m = folium.Map(location=start_location, zoom_start=10)  # Adjust zoom for larger areas

    # Plot the route on the map
    folium.PolyLine(locations=[(G.nodes[node]['y'], G.nodes[node]['x']) for node in route], color="purple", weight=5, opacity=0.8).add_to(m)
    
    # Add markers for the start and end locations
    folium.Marker(start_location, popup="Start").add_to(m)
    folium.Marker(end_location, popup="End").add_to(m)
    
    return m

start_location = (37.7749, -122.4194)  # San Francisco
end_location = (37.8, -122.427)    # Near San Francisco
map_obj = create_location_map(start_location, end_location)
map_obj.save("map.html")
