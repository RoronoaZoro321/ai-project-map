# gui.py

import tkinter as tk
from tkinter import messagebox
from pyswip import Prolog
from threading import Timer
import random


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
        self.obstacle_timer = None

        # Create GUI components
        self.create_widgets()

        # Start obstacle timer
        self.start_obstacle_timer()

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
        modes = ["car", "walking", "motorcycle", "airplane"]
        self.mode_menu = tk.OptionMenu(self.root, self.mode_var, *modes)
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

        # Label to display route information
        self.info_label = tk.Label(self.root, text="", justify="left")
        self.info_label.grid(row=4, column=0, columnspan=2, padx=10, pady=10)

        # Label to display the countdown timer
        self.timer_label = tk.Label(self.root, text="", font=("Helvetica", 16))
        self.timer_label.grid(row=5, column=0, columnspan=2, padx=10, pady=10)

    def find_route(self):
        # Cancel any existing countdown timer
        if self.countdown_timer:
            self.root.after_cancel(self.countdown_timer)
            self.countdown_timer = None

        start = self.start_entry.get().strip()
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
                # Start the countdown timer
                self.start_countdown(time_in_minutes)
            else:
                self.info_label.config(
                    text="No path could be found between the specified nodes."
                )
                self.timer_label.config(text="")
        except Exception as e:
            self.info_label.config(text=f"An error occurred: {e}")
            self.timer_label.config(text="")

    def display_route_info(self, path, distance, time_in_minutes):
        info = f"Path: {' -> '.join(path)}\nTotal Distance: {distance} km\nEstimated Time: {time_in_minutes:.2f} minutes"
        self.info_label.config(text=info)

    def start_countdown(self, time_in_minutes):
        # Convert minutes to seconds
        total_seconds = int(time_in_minutes * 60)
        self.update_timer(total_seconds)

    def update_timer(self, remaining_seconds):
        if remaining_seconds >= 0:
            mins, secs = divmod(remaining_seconds, 60)
            timer_text = f"Time Remaining: {mins:02d}:{secs:02d}"
            self.timer_label.config(text=timer_text)
            # Schedule the next update after 1 second
            self.countdown_timer = self.root.after(
                1000, self.update_timer, remaining_seconds - 1
            )
        else:
            self.timer_label.config(text="Completed!")

    def cancel_route(self):
        # Cancel any existing countdown timer
        if self.countdown_timer:
            self.root.after_cancel(self.countdown_timer)
            self.countdown_timer = None
        # Clear the information labels
        self.info_label.config(text="Route canceled.")
        self.timer_label.config(text="")
        # Optional: Reset the input fields
        # self.start_entry.delete(0, tk.END)
        # self.goal_entry.delete(0, tk.END)
        # self.mode_var.set('car')

    def introduce_obstacle(self):
        # Remove a random edge in Prolog
        try:
            list(self.prolog.query("remove_random_edge"))
            # Schedule the GUI update in the main thread
            self.root.after(0, self.handle_obstacle)
        except Exception as e:
            self.root.after(
                0,
                lambda: self.info_label.config(
                    text=f"An error occurred while introducing obstacle: {e}"
                ),
            )

    def handle_obstacle(self):
        # Update the information label to inform the user
        self.info_label.config(text="An obstacle has appeared! Recalculating route.")
        self.timer_label.config(text="")
        # Cancel any existing countdown timer
        if self.countdown_timer:
            self.root.after_cancel(self.countdown_timer)
            self.countdown_timer = None
        # Recalculate the route
        self.find_route()

    def start_obstacle_timer(self):
        interval = random.randint(15, 30)  # Random interval between 15 and 30 seconds
        self.obstacle_timer = Timer(interval, self.obstacle_timer_handler)
        self.obstacle_timer.start()

    def obstacle_timer_handler(self):
        self.introduce_obstacle()
        self.start_obstacle_timer()  # Restart the timer

    def run(self):
        self.root.protocol("WM_DELETE_WINDOW", self.on_closing)
        self.root.mainloop()

    def on_closing(self):
        if messagebox.askokcancel("Quit", "Do you want to quit?"):
            # Cancel the obstacle timer
            if self.obstacle_timer:
                self.obstacle_timer.cancel()
            # Cancel the countdown timer
            if self.countdown_timer:
                self.root.after_cancel(self.countdown_timer)
            self.root.destroy()
