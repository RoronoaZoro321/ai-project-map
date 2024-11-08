import folium
import geopandas as gpd
import osmnx as ox

def create_location_map(start_location, end_location):
    """
    Create a folium map showing the road route between two locations.
    
    Parameters:
    start_location (tuple): Latitude and longitude of the starting location
    end_location (tuple): Latitude and longitude of the ending location
    """
    # Get the road network between the two locations
    G = ox.graph_from_point(start_location, dist=1000, network_type='drive')
    G = ox.add_edge_speeds(G)
    G = ox.add_edge_travel_times(G)
    route = ox.shortest_path(G, orig=start_location, dest=end_location, weight='travel_time')
    
    # Create the folium map
    m = folium.Map(location=start_location, zoom_start=13)
    
    # Plot the route on the map
    folium.PolyLine(locations=[G.nodes[node]['xy'] for node in route], color="red", weight=5, opacity=0.5).add_to(m)
    
    # Add markers for the start and end locations
    folium.Marker(start_location, popup="Start").add_to(m)
    folium.Marker(end_location, popup="End").add_to(m)
    
    return m


start_location = (37.7749, -122.4194)  # San Francisco
end_location = (36.0522, -121.2437)    # Los Angeles
map_obj = create_location_map(start_location, end_location)
map_obj.save("map.html")