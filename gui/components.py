# gui/components.py

import tkinter as tk
from tkinter import ttk


class Components:
    def __init__(self, root):
        self.root = root
        self.create_widgets()

    def create_widgets(self):
        """
        Creates and places all GUI components within organized frames and applies styles.
        """
        # Define Frames
        self.input_frame = ttk.Frame(self.root, padding="10 10 10 10")
        self.input_frame.grid(row=0, column=0, sticky="nsew")

        self.control_frame = ttk.Frame(self.root, padding="10 10 10 10")
        self.control_frame.grid(row=1, column=0, sticky="nsew")

        self.display_frame = ttk.Frame(self.root, padding="10 10 10 10")
        self.display_frame.grid(row=2, column=0, sticky="nsew")

        self.metrics_frame = ttk.Frame(self.root, padding="10 10 10 10")
        self.metrics_frame.grid(row=3, column=0, sticky="nsew")

        self.status_frame = ttk.Frame(self.root, padding="10 10 10 10")
        self.status_frame.grid(row=4, column=0, sticky="nsew")

        # Configure grid weights
        self.root.columnconfigure(0, weight=1)
        for i in range(5):
            self.root.rowconfigure(i, weight=1)

        # Define Styles
        style = ttk.Style()
        style.configure("TLabel", font=("Arial", 10))
        style.configure("TButton", font=("Arial", 10, "bold"))
        style.configure("TEntry", font=("Arial", 10))
        style.configure("Info.TLabel", foreground="blue")
        style.configure("Error.TLabel", foreground="red")

        # Input Frame Widgets
        # Transportation Mode Selection
        self.transportation_mode_var = tk.StringVar(value="car")
        transportation_mode_label = ttk.Label(
            self.input_frame, text="Transportation Mode:", style="TLabel"
        )
        transportation_mode_label.grid(row=0, column=0, padx=5, pady=5, sticky="w")
        transportation_mode_dropdown = ttk.OptionMenu(
            self.input_frame,
            self.transportation_mode_var,
            "car",
            "car",
            "walking",
            "motorcycle",
        )
        transportation_mode_dropdown.grid(row=0, column=1, padx=5, pady=5, sticky="w")

        # Algorithm Selection
        self.algorithm_var = tk.StringVar(value="dijkstra")  # Initialize algorithm_var
        algorithm_label = ttk.Label(
            self.input_frame, text="Pathfinding Algorithm:", style="TLabel"
        )
        algorithm_label.grid(row=0, column=2, padx=5, pady=5, sticky="w")
        algorithm_dropdown = ttk.OptionMenu(
            self.input_frame, self.algorithm_var, "dijkstra", "dijkstra", "astar"
        )
        algorithm_dropdown.grid(row=0, column=3, padx=5, pady=5, sticky="w")

        # Start location entries
        start_lat_label = ttk.Label(
            self.input_frame, text="Start Latitude:", style="TLabel"
        )
        start_lat_label.grid(row=1, column=0, padx=5, pady=5, sticky="w")
        self.start_lat_entry = ttk.Entry(self.input_frame, width=20)
        self.start_lat_entry.grid(row=1, column=1, padx=5, pady=5, sticky="w")
        self.start_lat_entry.insert(0, "13.621244148739478")  # Default value

        start_lon_label = ttk.Label(
            self.input_frame, text="Start Longitude:", style="TLabel"
        )
        start_lon_label.grid(row=2, column=0, padx=5, pady=5, sticky="w")
        self.start_lon_entry = ttk.Entry(self.input_frame, width=20)
        self.start_lon_entry.grid(row=2, column=1, padx=5, pady=5, sticky="w")
        self.start_lon_entry.insert(0, "100.61953164076222")  # Default value

        # End location entries
        end_lat_label = ttk.Label(
            self.input_frame, text="End Latitude:", style="TLabel"
        )
        end_lat_label.grid(row=3, column=0, padx=5, pady=5, sticky="w")
        self.end_lat_entry = ttk.Entry(self.input_frame, width=20)
        self.end_lat_entry.grid(row=3, column=1, padx=5, pady=5, sticky="w")
        self.end_lat_entry.insert(0, "13.626794128718828")  # Default value

        end_lon_label = ttk.Label(
            self.input_frame, text="End Longitude:", style="TLabel"
        )
        end_lon_label.grid(row=4, column=0, padx=5, pady=5, sticky="w")
        self.end_lon_entry = ttk.Entry(self.input_frame, width=20)
        self.end_lon_entry.grid(row=4, column=1, padx=5, pady=5, sticky="w")
        self.end_lon_entry.insert(0, "100.6149335353964")  # Default value

        # Delay probability input
        delay_prob_label = ttk.Label(
            self.input_frame, text="Delay Probability (%):", style="TLabel"
        )
        delay_prob_label.grid(row=5, column=0, padx=5, pady=5, sticky="w")
        self.delay_prob_entry = ttk.Entry(self.input_frame, width=20)
        self.delay_prob_entry.grid(row=5, column=1, padx=5, pady=5, sticky="w")
        self.delay_prob_entry.insert(0, "10")  # Default to 10%

        # Control Frame Widgets
        # Compute button
        self.compute_button = ttk.Button(
            self.control_frame, text="Compute Shortest Path"
        )
        self.compute_button.grid(row=0, column=0, padx=5, pady=5)

        # Start traversal button
        self.start_traversal_button = ttk.Button(
            self.control_frame, text="Start Traversal"
        )
        self.start_traversal_button.grid(row=0, column=1, padx=5, pady=5)

        # Cancel traversal button
        self.cancel_traversal_button = ttk.Button(
            self.control_frame, text="Cancel Traversal", state="disabled"
        )
        self.cancel_traversal_button.grid(row=0, column=2, padx=5, pady=5)

        # Display Frame Widgets
        # Path display
        path_label = ttk.Label(
            self.display_frame, text="Computed Path:", style="TLabel"
        )
        path_label.grid(row=0, column=0, padx=5, pady=5, sticky="w")
        self.path_text = tk.Text(
            self.display_frame, height=10, width=80, font=("Arial", 10)
        )
        self.path_text.grid(row=1, column=0, padx=5, pady=5)

        # Metrics Frame Widgets
        # Total time label
        self.total_time_label = ttk.Label(
            self.metrics_frame, text="Total Time: 00:00", style="Info.TLabel"
        )
        self.total_time_label.grid(row=0, column=0, padx=5, pady=5, sticky="w")

        # Total distance label
        self.total_distance_label = ttk.Label(
            self.metrics_frame, text="Total Distance: 0.00 m", style="Info.TLabel"
        )
        self.total_distance_label.grid(row=0, column=1, padx=5, pady=5, sticky="w")

        # Remaining time label
        self.remaining_time_label = ttk.Label(
            self.metrics_frame, text="Remaining Time: 00:00", style="Info.TLabel"
        )
        self.remaining_time_label.grid(row=1, column=0, padx=5, pady=5, sticky="w")

        # Remaining distance label
        self.remaining_distance_label = ttk.Label(
            self.metrics_frame, text="Remaining Distance: 0.00 m", style="Info.TLabel"
        )
        self.remaining_distance_label.grid(row=1, column=1, padx=5, pady=5, sticky="w")

        # Status Frame Widgets
        # Status message label
        self.status_label = ttk.Label(
            self.status_frame, text="Status: N/A", style="Error.TLabel"
        )
        self.status_label.grid(row=0, column=0, padx=5, pady=5, sticky="w")

        # Information Log
        info_log_label = ttk.Label(
            self.status_frame, text="Traversal Information:", style="TLabel"
        )
        info_log_label.grid(row=1, column=0, padx=5, pady=5, sticky="w")
        self.info_log = tk.Text(
            self.status_frame,
            height=10,
            width=80,
            state="disabled",
            font=("Arial", 10),
            background="#f0f0f0",
            relief="sunken",
            borderwidth=2,
        )
        self.info_log.grid(row=2, column=0, padx=5, pady=5)

        # Configure column weights within frames for responsive design
        for frame in [
            self.input_frame,
            self.control_frame,
            self.display_frame,
            self.metrics_frame,
            self.status_frame,
        ]:
            frame.columnconfigure(0, weight=1)
            frame.columnconfigure(1, weight=1)
            frame.columnconfigure(2, weight=1)
            frame.columnconfigure(3, weight=1)
