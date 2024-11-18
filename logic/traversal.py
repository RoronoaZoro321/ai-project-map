# logic/traversal.py

import random  # Import the random module
import tkinter as tk
import time
from utils.utils import format_time


class Traversal:
    def __init__(self, map_instance, visualization, components, root, prolog_interface):
        self.map = map_instance
        self.visualization = visualization
        self.components = components
        self.root = root
        self.prolog_interface = prolog_interface
        self.cancelled = False
        self.delay_label_added = False
        self.total_delay_time = 0  # Initialize total delay time
        self.original_total_time = map_instance.total_time  # Store original total time

    def set_metrics_labels(self, total_delay_label, time_difference_label):
        """
        Sets the labels for Total Delay Time and Time Difference.
        """
        self.total_delay_label = total_delay_label
        self.time_difference_label = time_difference_label

    def start(self, delay_probability):
        """
        Starts the traversal process.
        """
        self.cancelled = False
        self.components.status_label.config(
            text="Traversal started.", style="Info.TLabel"
        )
        self.components.cancel_traversal_button.config(state="normal")

        # Assign delays via Prolog
        delayed_edges = self.prolog_interface.assign_delays(
            self.map.route, delay_probability
        )
        self.map.delayed_edges = delayed_edges  # Store in map_instance

        # Initialize traversal parameters
        self._traverse()

        self.log_info("Traversal started.")

    def _traverse(self):
        """
        Initializes traversal metrics and schedules the first update.
        """
        self.start_time = time.time()  # Initialize start_time here
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
            self.components.status_label.config(
                text="Traversal cancelled.", style="Error.TLabel"
            )
            self.log_info("Traversal cancelled.")
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
            self.components.remaining_time_label.config(text="Remaining Time: 00:00")
            self.components.remaining_distance_label.config(
                text="Remaining Distance: 0.00 m"
            )
            self.visualization.point_plot.set_data(
                self.visualization.x_data[-1], self.visualization.y_data[-1]
            )
            self.visualization.canvas.draw()
            self.components.status_label.config(
                text="Traversal completed.", style="Info.TLabel"
            )
            self.log_info("Traversal completed.")
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

        # Update time labels using format_time
        self.components.remaining_time_label.config(
            text=f"Remaining Time: {format_time(max(remaining_time, 0))}"
        )
        self.components.remaining_distance_label.config(
            text=f"Remaining Distance: {max(remaining_distance, 0):.2f} m"
        )

        current_node = route[idx]
        next_node = route[idx + 1]

        # Define the edge as a sorted tuple to handle undirected edges uniformly
        edge = tuple(sorted((current_node, next_node)))

        # Log current traversal
        self.log_info(f"Traversing from Node {current_node} to Node {next_node}.")

        # Check if this edge has a delay
        if (
            edge in map_instance.delayed_edges
            and edge not in map_instance.processed_edges
        ):
            # Mark this edge as processed
            map_instance.processed_edges.add(edge)

            # Define delay parameters with random delay_time
            delay_time = random.randint(
                10, 30
            )  # Random delay between 10 and 30 seconds
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
                text=f"Total Time: {format_time(map_instance.total_time)}"
            )
            self.components.total_distance_label.config(
                text=f"Total Distance: {map_instance.total_distance:.2f} m"
            )

            # Update the Total Delay Time
            self.total_delay_time += delay_time
            self.total_delay_label.config(
                text=f"Total Delay Time: {format_time(self.total_delay_time)}"
            )

            # Calculate Time Difference
            time_difference = map_instance.total_time - self.original_total_time
            self.time_difference_label.config(
                text=f"Time Difference: {format_time(time_difference)}"
            )

            # Update the status label with the randomized delay_time
            self.components.status_label.config(
                text=f"Delay occurred on edge ({current_node}, {next_node}). Traversal slowed down by {format_time(delay_time)}.",
                style="Error.TLabel",
            )
            self.log_info(
                f"Delay occurred on edge ({current_node}, {next_node}). Traversal slowed down by {format_time(delay_time)}."
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
                label=(
                    "Delayed Segment"
                    if not self.visualization.delay_label_added
                    else ""
                ),
            )

            # Add legend entry only once
            if not self.visualization.delay_label_added:
                self.visualization.ax.legend()
                self.visualization.delay_label_added = True

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
        self.components.status_label.config(
            text="Traversal cancelled.", style="Error.TLabel"
        )
        self.log_info("Traversal cancelled.")

    def log_info(self, message):
        """
        Appends a message to the information log.

        Args:
            message (str): The message to log.
        """
        self.components.info_log.configure(state="normal")
        if hasattr(self, "start_time") and self.start_time is not None:
            elapsed_since_start = format_time(time.time() - self.start_time)
            log_message = f"{elapsed_since_start} - {message}\n"
        else:
            log_message = (
                f"00:00 - {message}\n"  # Default time if start_time is not set
            )
        self.components.info_log.insert(tk.END, log_message)
        self.components.info_log.configure(state="disabled")
        self.components.info_log.see(tk.END)  # Auto-scroll to the end
