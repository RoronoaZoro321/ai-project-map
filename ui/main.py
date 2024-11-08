from tkinter import *

window = Tk()
window.geometry('300x450')
window.title("map")
# window.configure(bg='#f8ebff')

welcome_label = Label(window, text='Welcome to Smart Map !')
start_entry = Entry(window)
start_label = Label(window,text='Starting point')
des_ebtry = Entry(window,)
des_label = Label(window,text='Destination')
enter_button = Button(window,text="enter", width=10)
reset_button = Button(window,text="reset")

welcome_label.grid(row=0,columnspan=2)
start_label.grid(row=1, column=0, padx=10, pady=10)
start_entry.grid(row=1, column=1, padx=10, pady=10)
des_label.grid(row=2, column=0, padx=10, pady=10)
des_ebtry.grid(row=2, column=1, padx=10, pady=10)
reset_button.grid(row=3, column=0, padx=10, pady=10)
enter_button.grid(row=3, column=1, padx=10, pady=10)

mainloop()