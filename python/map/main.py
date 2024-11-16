from map import Map
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


