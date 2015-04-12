# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import numpy as np
from matplotlib import pyplot as plt

n_neigbors = 5
n = 50

x = np.random.random((n,))
err = np.random.random((n,)) * 0.3
y = np.sin(2 * np.pi * x) + err
plt.plot(x, y, "o")


x0 = 0.2
def knn(x, y, k, x0):
    dist = np.abs(x - x0)
    loc = dist.argsort()[0:k]
    return(np.mean(y[loc]))
    
y_hat = np.zeros((n, ), dtype = np.float_)
x_test = np.linspace(0, 1, n)
for i in range(n):
    y_hat[i] = knn(x, y, 5, x_test[i])
    
plt.figure()
plt.plot(x, y, 'o')
plt.hold(True)
plt.plot(x_test, y_hat)


##
n = 1000
u = np.hstack((np.random.randn(n, 2), np.zeros((n, 1))))
v = np.hstack((np.random.randn(n, 2) * 1.5 + 2, np.ones((n, 1))))
plt.figure()

dataset = np.vstack((u, v))

def knn(x, y, k, x0):
    dist = np.sum((x - x0) ** 2, axis = 1)
    loc = dist.argsort()[0:k]
    y_hat = np.mean(y[loc])
    return(y_hat)

n_grid = 300    
x_grid = np.linspace(np.min(dataset[:, 0]), np.max(dataset[:, 0]), n_grid)
y_grid = np.linspace(np.min(dataset[:, 1]), np.max(dataset[:, 1]), n_grid)

def find_boundry(k, n_grid):
    doundry = []
    y_est = np.zeros((n_grid,))
    for i in x_grid:
        ii = 0
        for j in y_grid:
            y_est[ii] = np.abs(knn(dataset[:, :2], dataset[:, 2], k, np.array([i, j])) - 0.5)
            ii += 1
        loc = np.argmin(y_est)
        doundry.append([x_grid[loc], y_grid[loc]])
    return(doundry)

 temp=np.array(temp)   
plt.figure()
plt.plot(u[:, 0], u[:, 1], 'bo')
plt.hold(True)
plt.plot(v[:, 0], v[:, 1], "ro")
plt.plot(temp[:, 0], temp[:, 1])




