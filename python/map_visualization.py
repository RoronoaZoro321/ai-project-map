# map_visualization.py

import networkx as nx
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt


class MapVisualizer:
    def __init__(self, master):
        self.master = master
        self.figure = plt.Figure(figsize=(6, 4), dpi=100)
        self.ax = self.figure.add_subplot(111)
        self.canvas = FigureCanvasTkAgg(self.figure, master=master)
        self.canvas.get_tk_widget().grid(row=8, column=0, columnspan=3)
        self.graph = nx.Graph()
        self.pos = None  # Position mapping for nodes
        self.current_path_edges = []
        self.traversal_times = []
        self.traversal_index = 0
        self.traversal_timer = None
        self.current_edge_progress = 0  # Progress along the current edge (in seconds)
        self.current_edge_time = (
            0  # Total time to traverse the current edge (in seconds)
        )

    def update_graph_edges(self, edges):
        self.graph.clear()
        # Add edges to the graph
        for u, v, w in edges:
            self.graph.add_edge(u, v, weight=w)
        # Compute positions only once or when the graph changes significantly
        if not self.pos or len(self.graph.nodes) != len(self.pos):
            self.pos = nx.spring_layout(self.graph)
        self.draw_base_map()

    def draw_base_map(self):
        self.ax.clear()
        nx.draw(
            self.graph,
            self.pos,
            ax=self.ax,
            with_labels=True,
            node_color="lightblue",
            edge_color="gray",
        )
        self.canvas.draw()

    def draw_path(self, path, traversal_times, start_index=0, progress_along_edge=0):
        # Cancel any existing traversal animation
        if self.traversal_timer:
            self.master.after_cancel(self.traversal_timer)
            self.traversal_timer = None
        self.current_path_edges = list(zip(path, path[1:]))
        self.traversal_times = traversal_times
        self.traversal_index = start_index
        self.current_edge_progress = progress_along_edge
        if self.traversal_index < len(self.traversal_times):
            self.current_edge_time = self.traversal_times[self.traversal_index]
        else:
            self.current_edge_time = 0
        self.animate_traversal()

    def animate_traversal(self):
        if self.traversal_index <= len(self.current_path_edges):
            self.ax.clear()
            # Draw the base graph
            nx.draw(
                self.graph,
                self.pos,
                ax=self.ax,
                with_labels=True,
                node_color="lightblue",
                edge_color="gray",
            )

            # Draw traversed edges
            traversed_edges = self.current_path_edges[: self.traversal_index]
            nx.draw_networkx_edges(
                self.graph,
                self.pos,
                edgelist=traversed_edges,
                ax=self.ax,
                edge_color="red",
                width=2,
            )

            # Highlight the current node or progress along edge
            if self.traversal_index < len(self.current_path_edges):
                node1, node2 = self.current_path_edges[self.traversal_index]
                # Compute the position along the edge based on progress
                pos1 = self.pos[node1]
                pos2 = self.pos[node2]
                progress_ratio = (
                    self.current_edge_progress / self.current_edge_time
                    if self.current_edge_time > 0
                    else 0
                )
                x = pos1[0] + (pos2[0] - pos1[0]) * progress_ratio
                y = pos1[1] + (pos2[1] - pos1[1]) * progress_ratio
                # Draw the current position
                self.ax.plot(x, y, "ro", markersize=10)
            else:
                current_node = self.current_path_edges[-1][1]
                nx.draw_networkx_nodes(
                    self.graph,
                    self.pos,
                    nodelist=[current_node],
                    ax=self.ax,
                    node_color="orange",
                )

            self.canvas.draw()

            # Determine the time until the next update
            update_interval = 1000  # Update every second

            # Update the progress along the current edge
            self.current_edge_progress += (
                update_interval / 1000
            )  # Convert milliseconds to seconds

            if self.current_edge_progress >= self.current_edge_time:
                # Move to the next edge
                self.traversal_index += 1
                self.current_edge_progress = 0
                if self.traversal_index < len(self.traversal_times):
                    self.current_edge_time = self.traversal_times[self.traversal_index]
                else:
                    self.current_edge_time = 0

            # Schedule the next update
            self.traversal_timer = self.master.after(
                update_interval, self.animate_traversal
            )
        else:
            # Traversal complete
            self.traversal_timer = None

    def get_current_position(self):
        # Returns the current node or position along the edge
        if self.traversal_index < len(self.current_path_edges):
            node1, node2 = self.current_path_edges[self.traversal_index]
            progress_ratio = (
                self.current_edge_progress / self.current_edge_time
                if self.current_edge_time > 0
                else 0
            )
            return node1, node2, progress_ratio
        elif self.traversal_index == len(self.current_path_edges):
            # At the last node
            return self.current_path_edges[-1][1], None, 0
        else:
            # Traversal complete
            return None, None, 0

    def set_traversal_speed(self, speed):
        # Not used in the new implementation since traversal times are based on edge times
        pass

    def stop_traversal(self):
        # Stop the traversal animation
        if self.traversal_timer:
            self.master.after_cancel(self.traversal_timer)
            self.traversal_timer = None

    def clear_map(self):
        # Cancel any existing traversal animation
        self.stop_traversal()
        self.ax.clear()
        self.canvas.draw()
