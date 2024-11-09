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
        self.canvas.get_tk_widget().grid(row=15, column=0, columnspan=2)
        self.graph = nx.Graph()
        self.pos = None  # Position mapping for nodes
        self.current_path_edges = []
        self.traversal_index = 0
        self.traversal_timer = None

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

    def draw_path(self, path):
        self.current_path_edges = list(zip(path, path[1:]))
        self.traversal_index = 0
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

            # Highlight the current node
            if self.traversal_index < len(self.current_path_edges):
                current_node = self.current_path_edges[self.traversal_index][0]
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

            self.traversal_index += 1
            # Schedule the next update
            self.master.after(
                1000, self.animate_traversal
            )  # Adjust the interval as needed
        else:
            # Traversal complete
            pass

    def clear_map(self):
        self.ax.clear()
        self.canvas.draw()
