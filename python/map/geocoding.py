import requests

class Geocoder:
    def __init__(self, api_key=None):
        self.api_key = "AIzaSyAePMbLePyEY5xlBPBc9FA6ZVHc1I7ivAg"
        self.url = "https://maps.googleapis.com/maps/api/geocode/json"

    def geocode(self, address):
        """
        Geocode an address to latitude and longitude.

        Parameters:
        - address (str): The address or location to geocode.

        Returns:
        - tuple: (latitude, longitude) or None if geocoding fails.
        """
        params = {
            'address': address,
            'key': self.api_key
        }
        
        response = requests.get(self.url, params=params)
        
        if response.status_code == 200:
            data = response.json()
            if data['status'] == 'OK':
                lat = data['results'][0]['geometry']['location']['lat']
                lng = data['results'][0]['geometry']['location']['lng']
                return lat, lng
            else:
                print("Geocoding failed: ", data)
                return None
        else:
            print("Request failed with status code:", response.status_code)
            return None

# # Example usage
# address = "1600 Pennsylvania Ave NW, Washington, DC"
# geocoder = Geocoder()
# coordinates = geocoder.geocode(address)
# if coordinates:
#     print(f"Latitude: {coordinates[0]}, Longitude: {coordinates[1]}")

