# logic/prolog_interface.py

from pyswip import Prolog
import os


class PrologInterface:
    def __init__(self, prolog_file_path):
        self.prolog = Prolog()
        if not os.path.exists(prolog_file_path):
            raise FileNotFoundError(f"Prolog file not found at {prolog_file_path}")
        self.prolog.consult(prolog_file_path)

    def clear_dynamic_facts(self):
        """
        Clears previous dynamic facts from Prolog.
        """
        self.prolog.query("retractall(node(_, _, _))")
        self.prolog.query("retractall(edge(_, _, _))")
        self.prolog.query("retractall(delayed_edge(_, _))")

    def assert_nodes(self, G):
        """
        Asserts all nodes from the graph into Prolog.
        """
        node_facts = [
            f"node({node_id}, {data['y']}, {data['x']})"
            for node_id, data in G.nodes(data=True)
        ]
        for fact in node_facts:
            self.prolog.assertz(fact)

    def assert_edges(self, G):
        """
        Asserts all edges from the graph into Prolog.
        """
        edge_facts = []
        for u, v, data in G.edges(data=True):
            weight = data.get("length", 1)
            edge_facts.append(f"edge({u}, {v}, {weight})")
            edge_facts.append(f"edge({v}, {u}, {weight})")
        for fact in edge_facts:
            self.prolog.assertz(fact)

    def compute_shortest_path(self, orig_node, dest_node, algorithm):
        """
        Computes the shortest path using the specified algorithm.
        """
        query = f"shortest_path({orig_node}, {dest_node}, {algorithm}, Path, Distance)"
        result = list(self.prolog.query(query))
        if result:
            path = list(result[0]["Path"])
            distance = result[0]["Distance"]
            return path, distance
        else:
            raise Exception("No path found")

    def assign_delays(self, path, delay_probability):
        """
        Assigns delays to edges in the path based on the delay probability.

        Args:
            path (list): List of node IDs representing the path.
            delay_probability (float): Probability of delay occurring on each edge.

        Returns:
            list of tuples: List of edges (u, v) that have delays.
        """
        # Convert path list to Prolog list syntax
        path_str = "[" + ", ".join(map(str, path)) + "]"
        # Prolog query: assign_delays(Path, DelayProbability, DelayedEdges)
        query = f"assign_delays({path_str}, {delay_probability}, DelayedEdges)."
        result = list(self.prolog.query(query))
        if result:
            delayed_edges = result[0]["DelayedEdges"]
            # Each delayed_edge is a list [U, V]
            delayed_edges_tuples = [tuple(edge) for edge in delayed_edges]
            return delayed_edges_tuples
        else:
            return []
