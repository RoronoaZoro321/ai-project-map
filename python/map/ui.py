import tkinter as tk
from tkinter import ttk
from map import Map  # Importing your Map class
from geocoding import Geocoder  # Importing your Geocoder class
import webbrowser
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

class MapViewerApp:
    def __init__(self):
        self.app = tk.Tk()
        self.app.title("Map Viewer")
        self.app.geometry("900x700")
        self.app.configure(bg="#e8f4f8")

        self.start_location = None
        self.end_location = None
        self.map_instance = None

        self.build_gui()

    def build_gui(self):
        self.create_header()
        self.crete_plot_box()
        # self.create_input_box()
        self.create_input_box_temp()
    
    def create_header(self):
        # Title label
        title_label = tk.Label(
            self.app, text="Map Viewer", font=("Helvetica", 20, "bold"), bg="#e8f4f8", fg="#333"
        )
        title_label.pack(pady=10)

    # def create_input_box(self):
    #     # Input frame
    #     input_frame = tk.Frame(self.app, bg="#e8f4f8")
    #     input_frame.pack(pady=10)

    #     tk.Label(input_frame, text="Start Location:", font=("Arial", 12), bg="#e8f4f8", fg="#2f4f7f").grid(row=0, column=0, padx=10, pady=5)
    #     self.start_input = ttk.Entry(input_frame, width=40)
    #     self.start_input.grid(row=0, column=1, padx=10, pady=5)

    #     tk.Label(input_frame, text="End Location:", font=("Arial", 12), bg="#e8f4f8", fg="#2f4f7f").grid(row=1, column=0, padx=10, pady=5)
    #     self.end_input = ttk.Entry(input_frame, width=40)
    #     self.end_input.grid(row=1, column=1, padx=10, pady=5)

    #     # Buttons
    #     button_frame = tk.Frame(self.app, bg="#e8f4f8")
    #     button_frame.pack(pady=10)

    #     ttk.Button(button_frame, text="Enter", command=self.load_map).grid(row=0, column=0, padx=10)
    #     ttk.Button(button_frame, text="View Real Map", command=self.view_real_map).grid(row=0, column=1, padx=10)
    #     ttk.Button(button_frame, text="View as Graph", command=self.view_graph_map).grid(row=0, column=2, padx=10)

    def create_input_box_temp(self):
        # Input frame
        input_frame = tk.Frame(self.app, bg="#e8f4f8")
        input_frame.pack(pady=10)

        tk.Label(input_frame, text="Start latlong:", font=("Arial", 12), bg="#e8f4f8", fg="#2f4f7f").grid(row=0, column=0, padx=10, pady=5)
        self.start_input = ttk.Entry(input_frame, width=40)
        self.start_input.grid(row=0, column=1, padx=10, pady=5)
        self.start_input.insert(0, "13.621244148739478, 100.61953164076222")

        tk.Label(input_frame, text="End latlong:", font=("Arial", 12), bg="#e8f4f8", fg="#2f4f7f").grid(row=1, column=0, padx=10, pady=5)
        self.end_input = ttk.Entry(input_frame, width=40)
        self.end_input.grid(row=1, column=1, padx=10, pady=5)
        self.end_input.insert(0, "13.626794128718828, 100.6149335353964")

        # Buttons
        button_frame = tk.Frame(self.app, bg="#e8f4f8")
        button_frame.pack(pady=10)

        ttk.Button(button_frame, text="Enter", command=self.load_map).grid(row=0, column=0, padx=10)
        ttk.Button(button_frame, text="View Real Map", command=self.view_real_map).grid(row=0, column=1, padx=10)
        ttk.Button(button_frame, text="View as Graph", command=self.view_graph_map).grid(row=0, column=2, padx=10)

    


    def crete_plot_box(self):
        # Plot area
        self.plot_frame = tk.Frame(self.app, bg="#ffffff", relief="ridge", borderwidth=2)
        self.plot_frame.pack(padx=10, pady=10, fill=tk.BOTH, expand=True)


    def load_map(self):
        geocoder = Geocoder()
        start_address = self.start_input.get()
        end_address = self.end_input.get()

        try:
            # self.start_location = geocoder.geocode(start_address)
            # self.end_location = geocoder.geocode(end_address)
            self.start_location = tuple(map(float, self.start_input.get().split(',')))
            self.end_location = tuple(map(float, self.end_input.get().split(',')))

            if not self.start_location or not self.end_location:
                raise ValueError("Invalid locations!")

            self.map_instance = Map(self.start_location, self.end_location)
            self.view_graph_map()
            tk.messagebox.showinfo("Success", "Map created successfully!")
        except Exception as e:
            tk.messagebox.showerror("Error", f"Failed to load map: {e}")

    def view_real_map(self):
        if not self.map_instance:
            tk.messagebox.showerror("Error", "Please load the map first.")
            return

        self.map_instance.create_map_with_folium("map_with_folium.html")
        webbrowser.open("map_with_folium.html")

    def view_graph_map(self):
        if not self.map_instance:
            tk.messagebox.showerror("Error", "Please load the map first.")
            return

        self.plot_graph()

    def plot_graph(self):
        self.map_instance.create_map_without_folium()  # Adjusts to show in Tkinter if needed
        plt.show()

    def run(self):
        self.app.mainloop()

if __name__ == "__main__":
    viewer_app = MapViewerApp()
    viewer_app.run()
