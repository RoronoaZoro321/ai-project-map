import tkinter as tk

root = tk.Tk()
frame = tk.Frame(root)
frame.pack()

label = tk.Label(frame, text="Hello World!")
label.pack()

button = tk.Button(frame, text="Exit", command=frame.quit)
button.pack()

root.mainloop()
