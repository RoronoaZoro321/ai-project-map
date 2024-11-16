# logic/traversal.py

import time
import random
import tkinter as tk


class Traversal:
    def __init__(self, map_instance, visualization, components, root):
        self.map = map_instance
        self.visualization = visualization
        self.components = components
        self.root = root
        self.cancelled = False
        self.delay_label_added = False  # Flag to track legend entry for delays

    def start(self):
        """
        Starts the traversal process.
        """
        self.cancelled = False
        self.components.status_label.config(text="Traversal started.")
        self.components.cancel_traversal_button.config(state="normal")
        self._traverse()

    def _traverse(self):
        """
        Initializes traversal metrics and schedules the first update.
        """
        self.start_time = time.time()
        self.current_index = 0
        self.elapsed_time = 0
        self.segment_colors = ["purple"] * (len(self.map.route) - 1)
        self.schedule_update()

    def schedule_update(self):
        """
        Schedules the next traversal update.
        """
        if not self.cancelled:
            self.root.after(100, self.update_traversal)

    def update_traversal(self):
        """
        Updates the traversal visualization and logic.
        """
        if self.cancelled:
            self.components.status_label.config(text="Traversal cancelled.")
            return

        current_time = time.time()
        delta_time = current_time - getattr(self, "last_update_time", current_time)
        self.last_update_time = current_time
        self.elapsed_time += delta_time

        map_instance = self.map
        route = map_instance.route
        G = map_instance.G
        cumulative_times = map_instance.cumulative_times
        idx = self.current_index

        if idx >= len(cumulative_times) - 1:
            # Traversal completed
            self.components.remaining_time_label.config(text="Remaining Time: 0.00 sec")
            self.components.remaining_distance_label.config(
                text="Remaining Distance: 0.00 m"
            )
            self.visualization.point_plot.set_data(
                self.visualization.x_data[-1], self.visualization.y_data[-1]
            )
            self.visualization.canvas.draw()
            self.components.status_label.config(text="Traversal completed.")
            self.components.cancel_traversal_button.config(state="disabled")
            return

        total_time = map_instance.total_time
        elapsed_time = self.elapsed_time
        remaining_time = total_time - elapsed_time
        remaining_distance = (
            map_instance.total_distance * (remaining_time / total_time)
            if total_time > 0
            else 0
        )

        self.components.remaining_time_label.config(
            text=f"Remaining Time: {max(remaining_time, 0):.2f} sec"
        )
        self.components.remaining_distance_label.config(
            text=f"Remaining Distance: {max(remaining_distance, 0):.2f} m"
        )

        current_node = route[idx]
        next_node = route[idx + 1]

        # Define the edge as a sorted tuple to handle undirected edges uniformly
        edge = tuple(sorted((current_node, next_node)))

        # Check if this edge has already been processed for delay
        if edge not in map_instance.processed_edges:
            # Mark this edge as processed
            map_instance.processed_edges.add(edge)

            # Decide whether to apply a delay based on probability
            delay_probability = map_instance.delay_probability
            delay_occurred = random.random() < delay_probability

            if delay_occurred:
                # Define delay parameters
                delay_time = 60  # seconds
                delay_distance = 0  # meters (modify if delays affect distance)

                # Apply delay effects
                map_instance.total_time += delay_time
                map_instance.total_distance += delay_distance

                # Update the traversal metrics to include the delay
                # Add delay_time to all cumulative_times from the current index onwards
                for i in range(idx + 1, len(map_instance.cumulative_times)):
                    map_instance.cumulative_times[i] += delay_time

                # Update the GUI labels to reflect the new total_time and total_distance
                self.components.total_time_label.config(
                    text=f"Total Time: {map_instance.total_time:.2f} sec"
                )
                self.components.total_distance_label.config(
                    text=f"Total Distance: {map_instance.total_distance:.2f} m"
                )

                # Update the status label
                self.components.status_label.config(
                    text=f"Delay occurred on edge ({current_node}, {next_node}). Traversal slowed down by {delay_time} seconds."
                )

                # Update the segment color to yellow
                self.segment_colors[idx] = "yellow"

                # Optionally, visualize the delay on the map
                self.visualization.ax.plot(
                    [
                        self.visualization.x_data[idx],
                        self.visualization.x_data[idx + 1],
                    ],
                    [
                        self.visualization.y_data[idx],
                        self.visualization.y_data[idx + 1],
                    ],
                    color="yellow",  # Changed from "orange" to "yellow"
                    linewidth=3,
                    label="Delayed Segment" if not self.delay_label_added else "",
                )

                # Add legend entry only once
                if not self.delay_label_added:
                    self.visualization.ax.legend()
                    self.delay_label_added = True

                self.visualization.canvas.draw()

        # Update the marker position based on elapsed time and traversal metrics
        if self.current_index < len(cumulative_times) - 1:
            time_prev = cumulative_times[self.current_index]
            time_curr = cumulative_times[self.current_index + 1]
            frac = (
                (self.elapsed_time - time_prev) / (time_curr - time_prev)
                if (time_curr - time_prev) != 0
                else 0
            )

            # Clamp fraction between 0 and 1
            frac = max(0, min(frac, 1))

            # Interpolate position
            x_prev = self.visualization.x_data[self.current_index]
            y_prev = self.visualization.y_data[self.current_index]
            x_curr = self.visualization.x_data[self.current_index + 1]
            y_curr = self.visualization.y_data[self.current_index + 1]
            x = x_prev + frac * (x_curr - x_prev)
            y = y_prev + frac * (y_curr - y_prev)

            # Update marker position
            self.visualization.point_plot.set_data(x, y)

        # Update line to show current segment with appropriate color
        if self.current_index < len(self.segment_colors):
            self.visualization.ax.plot(
                [
                    self.visualization.x_data[self.current_index],
                    self.visualization.x_data[self.current_index + 1],
                ],
                [
                    self.visualization.y_data[self.current_index],
                    self.visualization.y_data[self.current_index + 1],
                ],
                color=self.segment_colors[self.current_index],
                linewidth=3,
            )
            self.visualization.canvas.draw()

        # Move to the next segment if time allows
        if self.elapsed_time >= cumulative_times[self.current_index + 1]:
            # Permanently draw the segment
            self.visualization.ax.plot(
                [
                    self.visualization.x_data[self.current_index],
                    self.visualization.x_data[self.current_index + 1],
                ],
                [
                    self.visualization.y_data[self.current_index],
                    self.visualization.y_data[self.current_index + 1],
                ],
                color=self.segment_colors[self.current_index],
                linewidth=3,
            )
            self.current_index += 1

        # Schedule the next update
        self.schedule_update()

    def cancel(self):
        """
        Cancels the traversal.
        """
        self.cancelled = True
        self.components.cancel_traversal_button.config(state="disabled")
        self.components.status_label.config(text="Traversal cancelled.")


def calculate_traversal_metrics(map_instance):
    """
    Calculates distances, times, cumulative distances, and cumulative times for the path.
    """
    G = map_instance.G
    path = map_instance.route

    distances = []
    times = []
    cumulative_distances = [0]
    cumulative_times = [0]
    total_distance = 0
    total_time = 0

    for u, v in zip(path[:-1], path[1:]):
        # Handle potential missing edges due to delays
        if G.has_edge(u, v):
            data = G.get_edge_data(u, v)[0]
        elif G.has_edge(v, u):
            data = G.get_edge_data(v, u)[0]
        else:
            # If edge is missing, set default values
            data = {"length": 0, "travel_time": 0}
        length = data.get("length", 0)  # in meters
        travel_time = data.get("travel_time", 0)  # in seconds
        total_distance += length
        total_time += travel_time
        distances.append(length)
        times.append(travel_time)
        cumulative_distances.append(total_distance)
        cumulative_times.append(total_time)

    # Assign to map_instance
    map_instance.distances = distances
    map_instance.times = times
    map_instance.cumulative_distances = cumulative_distances
    map_instance.cumulative_times = cumulative_times
    map_instance.total_distance = total_distance
    map_instance.total_time = total_time
