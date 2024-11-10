# gui.py

import tkinter as tk
from tkinter import messagebox
from pyswip import Prolog
from map_visualization import MapVisualizer


class MapNavigationApp:
    def __init__(self):
        # Initialize Prolog
        self.prolog = Prolog()
        self.prolog.consult("../prolog/prolog_program.pl")

        # Initialize GUI
        self.root = tk.Tk()
        self.root.title("Map Navigation")

        # Initialize variables
        self.countdown_timer = None
        self.current_position = None  # To track current position during traversal
        self.total_remaining_time = 0  # Remaining traversal time in seconds

        # Initialize Map Visualizer
        self.map_visualizer = MapVisualizer(self.root)

        # Create GUI components
        self.create_widgets()

    def create_widgets(self):
        # Labels and Entry fields for Start and Goal nodes
        tk.Label(self.root, text="Start Node:").grid(row=0, column=0, sticky="e")
        self.start_entry = tk.Entry(self.root)
        self.start_entry.grid(row=0, column=1, padx=5, pady=5)

        tk.Label(self.root, text="Goal Node:").grid(row=1, column=0, sticky="e")
        self.goal_entry = tk.Entry(self.root)
        self.goal_entry.grid(row=1, column=1, padx=5, pady=5)

        # OptionMenu for selecting the mode of transportation
        tk.Label(self.root, text="Mode of Transport:").grid(row=2, column=0, sticky="e")
        self.mode_var = tk.StringVar(value="car")
        self.modes = ["car", "walking", "motorcycle", "airplane"]
        self.mode_menu = tk.OptionMenu(self.root, self.mode_var, *self.modes)
        self.mode_menu.grid(row=2, column=1, padx=5, pady=5)

        # Button to find the route
        self.find_button = tk.Button(
            self.root, text="Find Route", command=self.find_route
        )
        self.find_button.grid(row=3, column=0, pady=10)

        # Cancel button to stop the countdown
        self.cancel_button = tk.Button(
            self.root, text="Cancel", command=self.cancel_route
        )
        self.cancel_button.grid(row=3, column=1, pady=10)

        # Reset button to reset the graph
        self.reset_button = tk.Button(self.root, text="Reset", command=self.reset_graph)
        self.reset_button.grid(row=3, column=2, pady=10)

        # Transportation Controller Button
        self.transport_button = tk.Button(
            self.root,
            text="Transportation Controller",
            command=self.open_transport_controller,
        )
        self.transport_button.grid(row=4, column=0, columnspan=3, pady=10)

        # Frames for Controllers
        controllers_frame = tk.Frame(self.root)
        controllers_frame.grid(row=5, column=0, columnspan=3, padx=5, pady=5)

        # Obstacle Controller Frame
        obstacle_frame = tk.LabelFrame(controllers_frame, text="Obstacle Controller")
        obstacle_frame.pack(side="left", padx=5, pady=5)

        tk.Label(obstacle_frame, text="Node 1:").grid(row=0, column=0, sticky="e")
        self.obstacle_node1_entry = tk.Entry(obstacle_frame)
        self.obstacle_node1_entry.grid(row=0, column=1, padx=5, pady=5)
        tk.Label(obstacle_frame, text="Node 2:").grid(row=1, column=0, sticky="e")
        self.obstacle_node2_entry = tk.Entry(obstacle_frame)
        self.obstacle_node2_entry.grid(row=1, column=1, padx=5, pady=5)
        self.remove_edge_button = tk.Button(
            obstacle_frame, text="Remove Edge", command=self.remove_edge
        )
        self.remove_edge_button.grid(row=2, column=0, columnspan=2, pady=5)

        # Add Edge Controller Frame
        add_edge_frame = tk.LabelFrame(controllers_frame, text="Add Edge")
        add_edge_frame.pack(side="left", padx=5, pady=5)

        tk.Label(add_edge_frame, text="From Node:").grid(row=0, column=0, sticky="e")
        self.add_node1_entry = tk.Entry(add_edge_frame)
        self.add_node1_entry.grid(row=0, column=1, padx=5, pady=5)
        tk.Label(add_edge_frame, text="To Node:").grid(row=1, column=0, sticky="e")
        self.add_node2_entry = tk.Entry(add_edge_frame)
        self.add_node2_entry.grid(row=1, column=1, padx=5, pady=5)
        tk.Label(add_edge_frame, text="Distance:").grid(row=2, column=0, sticky="e")
        self.add_distance_entry = tk.Entry(add_edge_frame)
        self.add_distance_entry.grid(row=2, column=1, padx=5, pady=5)
        self.add_edge_button = tk.Button(
            add_edge_frame, text="Add Edge", command=self.add_edge
        )
        self.add_edge_button.grid(row=3, column=0, columnspan=2, pady=5)

        # Delay Controller Frame
        delay_frame = tk.LabelFrame(controllers_frame, text="Delay Controller")
        delay_frame.pack(side="left", padx=5, pady=5)

        tk.Label(delay_frame, text="Node 1:").grid(row=0, column=0, sticky="e")
        self.delay_node1_entry = tk.Entry(delay_frame)
        self.delay_node1_entry.grid(row=0, column=1, padx=5, pady=5)
        tk.Label(delay_frame, text="Node 2:").grid(row=1, column=0, sticky="e")
        self.delay_node2_entry = tk.Entry(delay_frame)
        self.delay_node2_entry.grid(row=1, column=1, padx=5, pady=5)
        tk.Label(delay_frame, text="Delay (km):").grid(row=2, column=0, sticky="e")
        self.delay_value_entry = tk.Entry(delay_frame)
        self.delay_value_entry.grid(row=2, column=1, padx=5, pady=5)
        self.add_delay_button = tk.Button(
            delay_frame, text="Add Delay", command=self.add_delay
        )
        self.add_delay_button.grid(row=3, column=0, columnspan=2, pady=5)
        self.remove_delay_button = tk.Button(
            delay_frame, text="Remove Delay", command=self.remove_delay
        )
        self.remove_delay_button.grid(row=4, column=0, columnspan=2, pady=5)

        # Label to display route information
        self.info_label = tk.Label(self.root, text="", justify="left")
        self.info_label.grid(row=6, column=0, columnspan=3, padx=10, pady=10)

        # Label to display the countdown timer
        self.timer_label = tk.Label(self.root, text="", font=("Helvetica", 16))
        self.timer_label.grid(row=7, column=0, columnspan=3, padx=10, pady=10)

        # Map visualization is handled by MapVisualizer (row=8)

    # ... (Include all the methods as in the previous version, with adjustments to remove cost-related code.)

    def open_transport_controller(self):
        # Create a new window for the transportation controller
        self.transport_window = tk.Toplevel(self.root)
        self.transport_window.title("Transportation Controller")

        # Fetch current modes from Prolog
        modes = self.get_modes_from_prolog()

        self.transport_entries = {}

        for idx, mode in enumerate(modes):
            mode_name = mode["Mode"]
            speed = mode["Speed"]

            tk.Label(self.transport_window, text=f"Mode: {mode_name}").grid(
                row=idx, column=0, padx=5, pady=5
            )

            tk.Label(self.transport_window, text="Speed (km/h):").grid(
                row=idx, column=1, sticky="e"
            )
            speed_entry = tk.Entry(self.transport_window)
            speed_entry.insert(0, str(speed))
            speed_entry.grid(row=idx, column=2, padx=5, pady=5)

            self.transport_entries[mode_name] = {"speed": speed_entry}

        # Update button
        update_button = tk.Button(
            self.transport_window,
            text="Update Modes",
            command=self.update_transport_modes,
        )
        update_button.grid(row=len(modes), column=0, columnspan=3, pady=10)

    def get_modes_from_prolog(self):
        # Retrieve the current modes from Prolog
        modes = []
        query = "mode(Mode, Speed)"
        for result in self.prolog.query(query):
            modes.append({"Mode": str(result["Mode"]), "Speed": result["Speed"]})
        return modes

    def update_transport_modes(self):
        # Update modes in Prolog based on user input
        for mode_name, entries in self.transport_entries.items():
            speed_str = entries["speed"].get()
            try:
                speed = float(speed_str)
                # Remove old mode
                query_remove = f"retractall(mode({mode_name}, _))"
                list(self.prolog.query(query_remove))
                # Add updated mode
                query_add = f"assert(mode({mode_name}, {speed}))"
                list(self.prolog.query(query_add))
            except ValueError:
                messagebox.showwarning(
                    "Input Error", f"Invalid input for mode {mode_name}."
                )
                return
        messagebox.showinfo("Success", "Transportation modes updated successfully.")
        self.transport_window.destroy()

    def find_route(self, start_node=None):
        # Cancel any existing countdown timer
        if self.countdown_timer:
            self.root.after_cancel(self.countdown_timer)
            self.countdown_timer = None

        if start_node is None:
            start = self.start_entry.get().strip()
        else:
            start = start_node

        goal = self.goal_entry.get().strip()
        mode = self.mode_var.get()

        if not start or not goal:
            self.info_label.config(text="Please enter both start and goal nodes.")
            return

        # Ensure nodes are lowercase to match Prolog atoms
        start = start.lower()
        goal = goal.lower()

        # Construct the Prolog query
        query = f"find_route({start}, {goal}, {mode}, Path, Distance, Time)"
        try:
            result = list(self.prolog.query(query))
            if result:
                path = [str(node) for node in result[0]["Path"]]
                distance = result[0]["Distance"]
                time_in_hours = result[0]["Time"]
                time_in_minutes = time_in_hours * 60  # Convert time to minutes
                self.display_route_info(path, distance, time_in_minutes)
                # Update the graph edges from Prolog
                edges = self.get_edges_from_prolog()
                self.map_visualizer.update_graph_edges(edges)
                # Calculate traversal times per edge
                traversal_times = self.calculate_traversal_times(path, mode)
                # Draw the path with animation, starting from current position if available
                start_index = 0
                progress_along_edge = 0

                if hasattr(self, "current_position") and self.current_position:
                    # Resume from current position
                    node1, node2, progress_ratio = self.current_position
                    # Find the index of the current edge in the new path
                    try:
                        edge_index = self.find_edge_index_in_path(path, node1, node2)
                        if edge_index is not None:
                            start_index = edge_index
                            # Adjust progress along the edge
                            total_edge_time = traversal_times[start_index]
                            progress_along_edge = total_edge_time * progress_ratio
                            # Adjust remaining time
                            remaining_time_seconds = (
                                sum(traversal_times[start_index:]) - progress_along_edge
                            )
                            self.total_remaining_time = remaining_time_seconds
                            # Start countdown from remaining time
                            self.start_countdown(
                                remaining_time_seconds / 60
                            )  # Convert seconds to minutes
                        else:
                            # Edge not in path, start from the current node
                            start_index = path.index(node1)
                            progress_along_edge = 0
                            remaining_time_seconds = sum(traversal_times[start_index:])
                            self.total_remaining_time = remaining_time_seconds
                            self.start_countdown(remaining_time_seconds / 60)
                    except ValueError:
                        # Node not in path, cannot proceed
                        self.info_label.config(
                            text="No valid path from current position."
                        )
                        self.timer_label.config(text="")
                        self.map_visualizer.clear_map()
                        return
                else:
                    # Start from beginning
                    remaining_time_seconds = sum(traversal_times)
                    self.total_remaining_time = remaining_time_seconds
                    self.start_countdown(remaining_time_seconds / 60)

                self.map_visualizer.draw_path(
                    path, traversal_times, start_index, progress_along_edge
                )
            else:
                self.info_label.config(
                    text="No path could be found between the specified nodes."
                )
                self.timer_label.config(text="")
                self.map_visualizer.clear_map()
        except Exception as e:
            self.info_label.config(text=f"An error occurred: {e}")
            self.timer_label.config(text="")
            self.map_visualizer.clear_map()

    def display_route_info(self, path, distance, time_in_minutes):
        info = (
            f"Path: {' -> '.join(path)}\n"
            f"Total Distance: {distance} km\n"
            f"Estimated Time: {time_in_minutes:.2f} minutes"
        )
        self.info_label.config(text=info)

    def calculate_traversal_times(self, path, mode):
        traversal_times = []
        mode_speed = self.get_mode_speed(mode)
        if mode_speed is None:
            messagebox.showerror("Error", "Invalid mode of transport.")
            return traversal_times
        # Build a dictionary for edge distances
        edge_distances = self.get_edge_distances()
        # Iterate over path edges
        for i in range(len(path) - 1):
            node1 = path[i]
            node2 = path[i + 1]
            # Get distance between node1 and node2
            distance = edge_distances.get((node1, node2)) or edge_distances.get(
                (node2, node1)
            )
            if distance is None:
                messagebox.showerror(
                    "Error", f"No distance found between {node1} and {node2}."
                )
                distance = 0
            # Calculate time in seconds
            time_in_hours = distance / mode_speed
            time_in_seconds = time_in_hours * 3600  # Convert hours to seconds
            traversal_times.append(time_in_seconds)
        return traversal_times

    def get_mode_speed(self, mode):
        # Query Prolog for mode(Mode, Speed)
        query = f"mode({mode}, Speed)"
        try:
            result = list(self.prolog.query(query))
            if result:
                speed = result[0]["Speed"]
                return speed
            else:
                return None
        except Exception as e:
            messagebox.showerror("Error", f"An error occurred: {e}")
            return None

    def get_edge_distances(self):
        edge_distances = {}
        query = "edge(Node1, Node2, Distance, Delay)"
        for result in self.prolog.query(query):
            node1 = str(result["Node1"])
            node2 = str(result["Node2"])
            distance = result["Distance"] + result["Delay"]
            edge_distances[(node1, node2)] = distance
        return edge_distances

    def remove_edge(self):
        node1 = self.obstacle_node1_entry.get().strip().lower()
        node2 = self.obstacle_node2_entry.get().strip().lower()

        if not node1 or not node2:
            messagebox.showwarning(
                "Input Error", "Please enter both nodes to remove an edge."
            )
            return

        # Check if the edge is currently being traversed
        current_position = self.map_visualizer.get_current_position()
        if current_position:
            ce_node1, ce_node2, _ = current_position
            if (node1 == ce_node1 and node2 == ce_node2) or (
                node1 == ce_node2 and node2 == ce_node1
            ):
                self.info_label.config(
                    text="Cannot remove edge that is currently being traversed."
                )
                return

        # Construct the Prolog query
        query = f"remove_edge({node1}, {node2})"
        try:
            result = list(self.prolog.query(query))
            if result:
                # Edge was successfully removed
                # Update the graph edges from Prolog
                edges = self.get_edges_from_prolog()
                self.map_visualizer.update_graph_edges(edges)
                # Inform the user
                self.info_label.config(
                    text=f"Edge between {node1} and {node2} has been removed."
                )
                # Check if the remaining path is affected
                self.check_and_update_route()
            else:
                # Edge does not exist
                self.info_label.config(
                    text="No edge exists between the specified nodes."
                )
        except Exception as e:
            self.info_label.config(text=f"An error occurred: {e}")

    def check_and_update_route(self):
        # Get current position from MapVisualizer
        current_position = self.map_visualizer.get_current_position()
        if current_position:
            node1, node2, progress_ratio = current_position
            self.current_position = (node1, node2, progress_ratio)
            # Recalculate the route from current position to goal
            if node2:
                # Currently traversing an edge, decide whether to use node1 or node2
                # For simplicity, we'll use the node closer to the goal
                start_node = node1 if progress_ratio < 0.5 else node2
            else:
                # At a node
                start_node = node1
            self.find_route(start_node=start_node)
        else:
            # No traversal in progress
            pass

    def add_edge(self):
        node1 = self.add_node1_entry.get().strip().lower()
        node2 = self.add_node2_entry.get().strip().lower()
        distance_str = self.add_distance_entry.get().strip()

        if not node1 or not node2 or not distance_str:
            messagebox.showwarning(
                "Input Error",
                "Please enter both nodes and the distance to add an edge.",
            )
            return

        try:
            distance = float(distance_str)
        except ValueError:
            messagebox.showwarning(
                "Input Error", "Please enter a valid number for distance."
            )
            return

        # Construct the Prolog query
        query = f"add_edge({node1}, {node2}, {distance})"
        try:
            list(self.prolog.query(query))
            # Update the graph edges from Prolog
            edges = self.get_edges_from_prolog()
            self.map_visualizer.update_graph_edges(edges)
            # Inform the user
            self.info_label.config(
                text=f"Edge added between {node1} and {node2} with distance {distance}."
            )
            # Recalculate the route
            self.find_route()
        except Exception as e:
            self.info_label.config(text=f"An error occurred: {e}")

    def add_delay(self):
        node1 = self.delay_node1_entry.get().strip().lower()
        node2 = self.delay_node2_entry.get().strip().lower()
        delay_str = self.delay_value_entry.get().strip()

        if not node1 or not node2 or not delay_str:
            messagebox.showwarning(
                "Input Error", "Please enter both nodes and the delay value."
            )
            return

        try:
            delay = float(delay_str)
            if delay < 0:
                raise ValueError("Delay must be non-negative.")
        except ValueError as e:
            messagebox.showwarning("Input Error", f"Invalid delay value: {e}")
            return

        # Construct the Prolog query
        query = f"add_delay({node1}, {node2}, {delay})"
        try:
            result = list(self.prolog.query(query))
            if result:
                # Delay was successfully added
                # Update the graph edges from Prolog
                edges = self.get_edges_from_prolog()
                self.map_visualizer.update_graph_edges(edges)
                # Inform the user
                self.info_label.config(
                    text=f"Delay of {delay} km added between {node1} and {node2}."
                )
                # Recalculate the route
                self.find_route()
            else:
                # Edge does not exist
                self.info_label.config(
                    text="No edge exists between the specified nodes."
                )
        except Exception as e:
            self.info_label.config(text=f"An error occurred: {e}")

    def remove_delay(self):
        node1 = self.delay_node1_entry.get().strip().lower()
        node2 = self.delay_node2_entry.get().strip().lower()

        if not node1 or not node2:
            messagebox.showwarning(
                "Input Error", "Please enter both nodes to remove a delay."
            )
            return

        # Construct the Prolog query
        query = f"remove_delay({node1}, {node2})"
        try:
            result = list(self.prolog.query(query))
            if result:
                # Delay was successfully removed
                # Update the graph edges from Prolog
                edges = self.get_edges_from_prolog()
                self.map_visualizer.update_graph_edges(edges)
                # Inform the user
                self.info_label.config(
                    text=f"Delay removed between {node1} and {node2}."
                )
                # Recalculate the route
                self.find_route()
            else:
                # Edge does not exist or no delay was present
                self.info_label.config(
                    text="No delay exists between the specified nodes."
                )
        except Exception as e:
            self.info_label.config(text=f"An error occurred: {e}")

    def get_edges_from_prolog(self):
        edges = []
        query = "edge(Node1, Node2, Distance, Delay)"
        for result in self.prolog.query(query):
            node1 = str(result["Node1"])
            node2 = str(result["Node2"])
            distance = result["Distance"] + result["Delay"]
            edges.append((node1, node2, distance))
        return edges

    def cancel_route(self):
        if self.countdown_timer:
            self.root.after_cancel(self.countdown_timer)
            self.countdown_timer = None
        self.info_label.config(text="Route canceled.")
        self.timer_label.config(text="")
        self.map_visualizer.clear_map()
        self.current_position = None

    def reset_graph(self):
        if self.countdown_timer:
            self.root.after_cancel(self.countdown_timer)
            self.countdown_timer = None
        list(self.prolog.query("reset_all"))
        self.map_visualizer.clear_map()
        self.info_label.config(text="Graph has been reset to initial state.")
        self.timer_label.config(text="")
        self.current_position = None

    def start_countdown(self, time_in_minutes):
        total_seconds = int(time_in_minutes * 60)
        self.update_timer(total_seconds)

    def update_timer(self, remaining_seconds):
        if remaining_seconds >= 0:
            mins, secs = divmod(remaining_seconds, 60)
            timer_text = f"Time Remaining: {mins:02d}:{secs:02d}"
            self.timer_label.config(text=timer_text)
            self.total_remaining_time = remaining_seconds
            self.countdown_timer = self.root.after(
                1000, self.update_timer, remaining_seconds - 1
            )
        else:
            self.timer_label.config(text="Completed!")
            self.current_position = (
                None  # Reset current position when traversal is complete
            )

    def find_edge_index_in_path(self, path, node1, node2):
        for index, (n1, n2) in enumerate(zip(path, path[1:])):
            if (n1 == node1 and n2 == node2) or (n1 == node2 and n2 == node1):
                return index
        return None

    def run(self):
        self.root.protocol("WM_DELETE_WINDOW", self.on_closing)
        self.root.mainloop()

    def on_closing(self):
        if messagebox.askokcancel("Quit", "Do you want to quit?"):
            if self.countdown_timer:
                self.root.after_cancel(self.countdown_timer)
            self.root.destroy()


# To run the application, ensure that you have an entry point like this:
if __name__ == "__main__":
    app = MapNavigationApp()
    app.run()
