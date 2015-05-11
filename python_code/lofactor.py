# -*- coding: utf-8 -*-
"""
Created on Mon May 11 16:22:01 2015

@author: wang
"""

import numpy as np
import matplotlib.pyplot as plt

def get_dist(mat):
    nrow = mat.shape[0]
    dist = np.zeros((nrow, nrow))
    for i in range(nrow):
        dist[i,:] = np.sqrt(np.sum((mat - mat[i, :]) ** 2, axis = 1))
    return dist
    
def k_distance(dist, k):  ##可以同时计算k_distance和knn
    k_dist = np.sort(dist, axis = 1)[:,k]
    return k_dist
    
def reachable_dist(dist, k_dist, i, j):
    rch_dist = max(dist[i, j], k_dist[j])
    return rch_dist

def rch_knn(dist, k_dist, i):
    nrow = k_dist.shape[0]
    knn = [j for j in range(nrow) if (dist[i, j] != 0 and dist[i, j] <= k_dist[i])]
    return knn
    
def ird(dist, k_dist):
    nrow = k_dist.shape[0]
    ird = np.zeros(nrow)
    for i in range(nrow):
        knn = rch_knn(dist, k_dist, i)
        ird[i] = 1 / np.mean([reachable_dist(dist, k_dist, i, j) for j in knn])
    return ird

def lof(dist, k_dist):
    nrow = k_dist.shape[0]
    irds = ird(dist, k_dist)
    lof = np.zeros(nrow)
    for i in range(nrow):
        knn = rch_knn(dist, k_dist, i)
        lof[i] = np.mean(irds[knn]) / irds[i]
    return lof
        
n = 3000
data1 = np.random.randn(n, 10)
data2 = np.random.randn(n, 10) * 1.5 + 5
data = np.vstack((data1, data2))    

data = np.arange(1, 5001).reshape(1250, 4)
    
dist = get_dist(data)
k_dist = k_distance(dist, 4)
lof1 = lof(dist, k_dist)

outlier = data[lof1 > 2, :]
plt.figure()
plt.plot(data[:,0], data[:, 1], 'o')
plt.hold(True)
plt.plot(outlier[:,0], outlier[:, 1], 'ro')