# gui/components.py

import tkinter as tk
from tkinter import ttk


class Components:
    def __init__(self, root):
        self.root = root
        self.create_widgets()

    def create_widgets(self):
        """
        Creates and places all GUI components.
        """
        # Algorithm selection
        self.algorithm_var = tk.StringVar(value="dijkstra")
        algorithm_label = ttk.Label(self.root, text="Select Algorithm:")
        algorithm_label.grid(row=0, column=0, padx=5, pady=5, sticky="w")
        algorithm_dropdown = ttk.OptionMenu(
            self.root, self.algorithm_var, "dijkstra", "dijkstra", "astar"
        )
        algorithm_dropdown.grid(row=0, column=1, padx=5, pady=5, sticky="w")

        # Start location entries
        start_lat_label = ttk.Label(self.root, text="Start Latitude:")
        start_lat_label.grid(row=1, column=0, padx=5, pady=5, sticky="w")
        self.start_lat_entry = ttk.Entry(self.root)
        self.start_lat_entry.grid(row=1, column=1, padx=5, pady=5, sticky="w")
        self.start_lat_entry.insert(0, "13.621244148739478")  # Default value

        start_lon_label = ttk.Label(self.root, text="Start Longitude:")
        start_lon_label.grid(row=2, column=0, padx=5, pady=5, sticky="w")
        self.start_lon_entry = ttk.Entry(self.root)
        self.start_lon_entry.grid(row=2, column=1, padx=5, pady=5, sticky="w")
        self.start_lon_entry.insert(0, "100.61953164076222")  # Default value

        # End location entries
        end_lat_label = ttk.Label(self.root, text="End Latitude:")
        end_lat_label.grid(row=3, column=0, padx=5, pady=5, sticky="w")
        self.end_lat_entry = ttk.Entry(self.root)
        self.end_lat_entry.grid(row=3, column=1, padx=5, pady=5, sticky="w")
        self.end_lat_entry.insert(0, "13.626794128718828")  # Default value

        end_lon_label = ttk.Label(self.root, text="End Longitude:")
        end_lon_label.grid(row=4, column=0, padx=5, pady=5, sticky="w")
        self.end_lon_entry = ttk.Entry(self.root)
        self.end_lon_entry.grid(row=4, column=1, padx=5, pady=5, sticky="w")
        self.end_lon_entry.insert(0, "100.6149335353964")  # Default value

        # Compute button
        self.compute_button = ttk.Button(self.root, text="Compute Shortest Path")
        self.compute_button.grid(row=5, column=0, columnspan=2, padx=5, pady=10)

        # Path display
        path_label = ttk.Label(self.root, text="Computed Path:")
        path_label.grid(row=6, column=0, padx=5, pady=5, sticky="w")
        self.path_text = tk.Text(self.root, height=10, width=50)
        self.path_text.grid(row=7, column=0, columnspan=2, padx=5, pady=5)

        # Delay probability input
        delay_prob_label = ttk.Label(self.root, text="Delay Probability (%):")
        delay_prob_label.grid(row=8, column=0, padx=5, pady=5, sticky="w")
        self.delay_prob_entry = ttk.Entry(self.root)
        self.delay_prob_entry.grid(row=8, column=1, padx=5, pady=5, sticky="w")
        self.delay_prob_entry.insert(0, "10")  # Default to 10%

        # Start traversal button
        self.start_traversal_button = ttk.Button(self.root, text="Start Traversal")
        self.start_traversal_button.grid(row=9, column=0, padx=5, pady=10)

        # Cancel traversal button
        self.cancel_traversal_button = ttk.Button(
            self.root, text="Cancel Traversal", state="disabled"
        )
        self.cancel_traversal_button.grid(row=9, column=1, padx=5, pady=10)

        # Total time label
        self.total_time_label = ttk.Label(self.root, text="Total Time: N/A")
        self.total_time_label.grid(row=10, column=0, padx=5, pady=5, sticky="w")

        # Total distance label
        self.total_distance_label = ttk.Label(self.root, text="Total Distance: N/A")
        self.total_distance_label.grid(row=11, column=0, padx=5, pady=5, sticky="w")

        # Remaining time label
        self.remaining_time_label = ttk.Label(self.root, text="Remaining Time: N/A")
        self.remaining_time_label.grid(row=10, column=1, padx=5, pady=5, sticky="w")

        # Remaining distance label
        self.remaining_distance_label = ttk.Label(
            self.root, text="Remaining Distance: N/A"
        )
        self.remaining_distance_label.grid(row=11, column=1, padx=5, pady=5, sticky="w")

        # Status message label
        self.status_label = ttk.Label(self.root, text="Status: N/A")
        self.status_label.grid(
            row=13, column=0, columnspan=2, padx=5, pady=5, sticky="w"
        )
