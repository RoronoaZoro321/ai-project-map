# gui/components.py

import tkinter as tk
from tkinter import ttk, messagebox


class Components:
    def __init__(self, parent):
        self.parent = parent
        self.create_widgets()

    def create_widgets(self):
        """
        Creates and places all GUI components within organized frames and applies styles.
        """
        # Define Frames within the right_frame
        self.input_frame = ttk.LabelFrame(
            self.parent, text="Inputs", padding="10 10 10 10"
        )
        self.input_frame.grid(row=0, column=0, padx=5, pady=5, sticky="nsew")

        self.control_frame = ttk.LabelFrame(
            self.parent, text="Controls", padding="10 10 10 10"
        )
        self.control_frame.grid(row=1, column=0, padx=5, pady=5, sticky="nsew")

        self.map_display_frame = ttk.LabelFrame(
            self.parent, text="Map Display", padding="10 10 10 10"
        )
        self.map_display_frame.grid(row=2, column=0, padx=5, pady=5, sticky="nsew")

        # Remove or Comment Out the Display Frame
        # self.display_frame = ttk.LabelFrame(
        #     self.parent, text="Path Display", padding="10 10 10 10"
        # )
        # self.display_frame.grid(row=3, column=0, padx=5, pady=5, sticky="nsew")

        self.metrics_frame = ttk.LabelFrame(
            self.parent, text="Metrics", padding="10 10 10 10"
        )
        self.metrics_frame.grid(row=3, column=0, padx=5, pady=5, sticky="nsew")

        self.status_frame = ttk.LabelFrame(
            self.parent, text="Status", padding="10 10 10 10"
        )
        self.status_frame.grid(row=4, column=0, padx=5, pady=5, sticky="nsew")

        # Configure grid weights for responsiveness
        self.parent.columnconfigure(0, weight=1)
        for i in range(5):
            self.parent.rowconfigure(i, weight=1)

        # Define Styles
        style = ttk.Style()
        style.configure("TLabel", font=("Arial", 10))
        style.configure("TButton", font=("Arial", 10, "bold"))
        style.configure("TEntry", font=("Arial", 10))
        style.configure("Info.TLabel", foreground="blue")
        style.configure("Error.TLabel", foreground="red")

        # Input Frame Widgets
        # Transportation Mode and Pathfinding Algorithm (Same Line)
        self.transportation_mode_var = tk.StringVar(value="car")
        transportation_mode_label = ttk.Label(
            self.input_frame, text="Transportation Mode:", style="TLabel"
        )
        transportation_mode_label.grid(row=0, column=0, padx=5, pady=5, sticky="e")
        transportation_mode_dropdown = ttk.OptionMenu(
            self.input_frame,
            self.transportation_mode_var,
            "car",
            "car",
            "walking",
            "motorcycle",
        )
        transportation_mode_dropdown.grid(row=0, column=1, padx=5, pady=5, sticky="w")

        self.algorithm_var = tk.StringVar(value="dijkstra")  # Initialize algorithm_var
        algorithm_label = ttk.Label(
            self.input_frame, text="Pathfinding Algorithm:", style="TLabel"
        )
        algorithm_label.grid(row=0, column=2, padx=5, pady=5, sticky="e")
        algorithm_dropdown = ttk.OptionMenu(
            self.input_frame, self.algorithm_var, "dijkstra", "dijkstra", "astar"
        )
        algorithm_dropdown.grid(row=0, column=3, padx=5, pady=5, sticky="w")

        # Node Input Entries (Aligned on the Same Line)
        lat_label = ttk.Label(self.input_frame, text="Latitude:", style="TLabel")
        lat_label.grid(row=1, column=0, padx=5, pady=5, sticky="e")

        self.lat_entry = ttk.Entry(self.input_frame, width=15)
        self.lat_entry.grid(row=1, column=1, padx=5, pady=5, sticky="w")
        self.lat_entry.insert(0, "13.621244")  # Default value

        lon_label = ttk.Label(self.input_frame, text="Longitude:", style="TLabel")
        lon_label.grid(row=1, column=2, padx=5, pady=5, sticky="e")

        self.lon_entry = ttk.Entry(self.input_frame, width=15)
        self.lon_entry.grid(row=1, column=3, padx=5, pady=5, sticky="w")
        self.lon_entry.insert(0, "100.619532")  # Default value

        # Add Node Button
        self.add_node_button = ttk.Button(self.input_frame, text="Add Node")
        self.add_node_button.grid(
            row=2, column=0, columnspan=4, padx=5, pady=5, sticky="ew"
        )

        # Remove Node Button
        self.remove_node_button = ttk.Button(
            self.input_frame, text="Remove Selected Node"
        )
        self.remove_node_button.grid(
            row=3, column=0, columnspan=4, padx=5, pady=5, sticky="ew"
        )

        # Listbox to Display Added Nodes
        nodes_label = ttk.Label(self.input_frame, text="Added Nodes:", style="TLabel")
        nodes_label.grid(row=4, column=0, padx=5, pady=5, sticky="w")
        self.nodes_listbox = tk.Listbox(
            self.input_frame, height=5, width=50, font=("Arial", 10)
        )
        self.nodes_listbox.grid(
            row=5, column=0, columnspan=4, padx=5, pady=5, sticky="nsew"
        )

        # Delay probability input
        delay_prob_label = ttk.Label(
            self.input_frame, text="Delay Probability (%):", style="TLabel"
        )
        delay_prob_label.grid(row=6, column=0, padx=5, pady=5, sticky="w")
        self.delay_prob_entry = ttk.Entry(self.input_frame, width=20)
        self.delay_prob_entry.grid(row=6, column=1, padx=5, pady=5, sticky="w")
        self.delay_prob_entry.insert(0, "10")  # Default to 10%

        # Control Frame Widgets
        # Compute button
        self.compute_button = ttk.Button(
            self.control_frame, text="Compute Shortest Path"
        )
        self.compute_button.grid(row=0, column=0, padx=5, pady=5, sticky="ew")

        # Start traversal button
        self.start_traversal_button = ttk.Button(
            self.control_frame, text="Start Traversal", state="disabled"
        )
        self.start_traversal_button.grid(row=0, column=1, padx=5, pady=5, sticky="ew")

        # Cancel traversal button
        self.cancel_traversal_button = ttk.Button(
            self.control_frame, text="Cancel Traversal", state="disabled"
        )
        self.cancel_traversal_button.grid(row=0, column=2, padx=5, pady=5, sticky="ew")

        # Map Display Frame Widgets
        # View Real Map button
        self.view_real_map_button = ttk.Button(
            self.map_display_frame, text="View as Map"
        )
        self.view_real_map_button.grid(row=0, column=0, padx=5, pady=5, sticky="ew")
        # View Graph button
        self.view_graph_button = ttk.Button(
            self.map_display_frame, text="View as Graph"
        )
        self.view_graph_button.grid(row=0, column=1, padx=5, pady=5, sticky="ew")

        self.view_detail_graph_button = ttk.Button(
            self.map_display_frame, text="View Detailed Graph"
        )
        self.view_detail_graph_button.grid(row=0, column=2, padx=5, pady=5, sticky="ew")

        self.clear_route_button = ttk.Button(self.map_display_frame, text="Clear Route")
        self.clear_route_button.grid(row=0, column=3, padx=5, pady=5, sticky="ew")

        # Remove Path Display Widgets
        # Commenting out the Path Display Label and Text Box
        # path_label = ttk.Label(
        #     self.display_frame, text="Computed Path:", style="TLabel"
        # )
        # path_label.grid(row=0, column=0, padx=5, pady=5, sticky="w")
        # self.path_text = tk.Text(
        #     self.display_frame, height=10, width=50, font=("Arial", 10)
        # )
        # self.path_text.grid(row=1, column=0, padx=5, pady=5, sticky="nsew")

        # Metrics Frame Widgets
        # Row 0: Total Time, Total Distance, Total Delay Time
        self.total_time_label = ttk.Label(
            self.metrics_frame, text="Total Time: 00:00", style="Info.TLabel"
        )
        self.total_time_label.grid(row=0, column=0, padx=5, pady=5, sticky="w")

        self.total_distance_label = ttk.Label(
            self.metrics_frame, text="Total Distance: 0.00 m", style="Info.TLabel"
        )
        self.total_distance_label.grid(row=0, column=1, padx=5, pady=5, sticky="w")

        self.total_delay_label = ttk.Label(
            self.metrics_frame, text="Total Delay Time: 00:00", style="Info.TLabel"
        )
        self.total_delay_label.grid(row=0, column=2, padx=5, pady=5, sticky="w")

        # Row 1: Remaining Time, Remaining Distance, Time Difference
        self.remaining_time_label = ttk.Label(
            self.metrics_frame, text="Remaining Time: 00:00", style="Info.TLabel"
        )
        self.remaining_time_label.grid(row=1, column=0, padx=5, pady=5, sticky="w")

        self.remaining_distance_label = ttk.Label(
            self.metrics_frame, text="Remaining Distance: 0.00 m", style="Info.TLabel"
        )
        self.remaining_distance_label.grid(row=1, column=1, padx=5, pady=5, sticky="w")

        self.time_difference_label = ttk.Label(
            self.metrics_frame, text="Time Difference: 00:00", style="Info.TLabel"
        )
        self.time_difference_label.grid(row=1, column=2, padx=5, pady=5, sticky="w")

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
            width=100,
            state="disabled",
            font=("Arial", 10),
            background="#000000",
            foreground="#00FF00",
            relief="sunken",
            borderwidth=2,
        )
        self.info_log.grid(row=2, column=0, padx=5, pady=5, sticky="nsew")

        # Configure column weights within frames for responsive design
        for frame in [
            self.input_frame,
            self.control_frame,
            self.map_display_frame,
            self.metrics_frame,
            self.status_frame,
        ]:
            frame.columnconfigure(0, weight=1)
            frame.columnconfigure(1, weight=1)
            frame.columnconfigure(2, weight=1)
            frame.columnconfigure(3, weight=1)

        # Configure row and column weights for Status Frame to stretch info_log
        self.status_frame.rowconfigure(2, weight=1)
        self.status_frame.columnconfigure(0, weight=1)

        # Optionally, configure row weights if needed
        self.metrics_frame.rowconfigure(0, weight=1)
        self.metrics_frame.rowconfigure(1, weight=1)
        # self.display_frame.rowconfigure(1, weight=1)
        # self.status_frame.rowconfigure(2, weight=1)

        # Additional Widgets for Node Management
        # These widgets have already been added above: Add Node, Remove Node, and Listbox
