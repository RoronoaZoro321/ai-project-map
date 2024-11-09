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
        self.canvas.get_tk_widget().grid(row=4, column=0, columnspan=2)
        self.graph = nx.Graph()
        self.pos = None  # Position mapping for nodes

    def update_graph_edges(self, edges):
        self.graph.clear()
        # Add edges to the graph
        for u, v, w in edges:
            self.graph.add_edge(u, v, weight=w)
        # Compute positions only once or when the graph changes significantly
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

    def draw_map(self, path):
        self.draw_base_map()
        # Highlight the path
        edge_list = list(zip(path, path[1:]))
        nx.draw_networkx_edges(
            self.graph,
            self.pos,
            edgelist=edge_list,
            ax=self.ax,
            edge_color="red",
            width=2,
        )
        nx.draw_networkx_nodes(
            self.graph, self.pos, nodelist=path, ax=self.ax, node_color="orange"
        )
        self.canvas.draw()

    def clear_map(self):
        self.ax.clear()
        self.canvas.draw()
