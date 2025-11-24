# -*- coding: utf-8 -*-
"""
Created on Thu Nov 20 14:58:13 2025
@author: andrearaaschou
"""
import numpy as np
import  matplotlib.pyplot as plt
from scipy.integrate import solve_ivp

t0 = 0
t_max = 1

def ode_solver(f, t0, y0, h, t_max):
    # Make dataframe to hold t- and y-values (t same as x in previous solution)
    sol = np.array([[t0, y0]])
    i = 0
    
    while sol[i, 0] < t_max:
        t_next = sol[i, 0] + h
        y_next = sol[i, 1] + h * fun(sol[i, 0], sol[i, 1])
        sol = np.vstack((sol, [t_next, y_next]))
        i += 1
        
    return sol
    
def fun(t, y):
    dydt = 2*y
    return dydt        

 
def ode_plot(my_sol, scipy_sol):
    # Extract t and y-values
    my_t = my_sol[:, 0]  
    my_y = my_sol[:, 1] 
    
    t_scipy = scipy_sol.t
    y_scipy = scipy_sol.y[0]
    
    plt.plot(my_t, my_y, label='My solution')
    plt.plot(t_scipy, y_scipy, label='Scipy solution')
    plt.xlabel('t')
    plt.ylabel('y(t)')
    plt.title('ODE Solution')
    plt.legend()
    plt.show()
    


my_sol = ode_solver(fun, t0, 1, 0.01, t_max)  
scipy_sol = solve_ivp(fun, t_span = [t0, t_max], y0 = [1], method='RK45',rtol=1e-6,          # smaller relative tolerance
    atol=1e-9 )
ode_plot(my_sol, scipy_sol)


# Find built in function to compare with (use scipy integrate solve_ivp) RK45 = Runge-Kutta method
# plot both solutions in the same blot as different lines