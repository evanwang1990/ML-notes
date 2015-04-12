# -*- coding: utf-8 -*-
"""
Created on Mon Mar 16 17:54:35 2015

数据挖掘第三次课程 

@author: wangjinling
"""

import numpy as np
from matplotlib import pyplot as plt

#part 1:kernel density方法
x1=np.random.randn(20,)
x2=np.random.randn(40,) * 1.5 + 4

x=np.hstack((x1, x2))

def kernel(h, u, x):
    return(np.mean(np.exp(- ((x - u) / h) ** 2 / 2) / (h * np.sqrt(2 * np.pi))))

density = np.zeros(500, )
x_density = np.linspace(x.min(), x.max(), 500)

h=0.5
for i in range(500):
    density[i] = kernel(h, x_density[i], x)

plt.plot(density)

#kmeans聚类
data = np.vstack((np.random.randn(100,2),
                  np.random.randn(200,2) * 3 + 4))
plt.plot(data[:,0], data[:,1], 'o')

def dist(x, points):
    return(np.sum((points - x)**2, axis = 1))

def kmeans(center, k, X):
    n = X.shape[0]
    center_new = center
    center_shift = 1
    dist_matrix = np.zeros((n, k))
    while center_shift > 0.05:
        for i in range(k):
            dist_matrix[:, i] = np.sum((X - center[i, :]) ** 2, axis = 1)
        idx = dist_matrix.argmin(axis = 1)
        for j in range(k):
            center_new[j, :] = np.mean(X[idx == j, :], axis = 0)
        center_shift = np.sum((center_new - center) ** 2)
        center = center_new
    return(idx, center)

k = 3
center = np.random.random((k, 2)) + 1

idx, centers = kmeans(center, k, data)

plt.figure()
plt.plot(data[idx==0, 0], data[idx==0, 1], 'bo')
plt.hold(True)
plt.plot(data[idx==1, 0], data[idx==1, 1], 'ro')
plt.plot(data[idx==2, 0], data[idx==2, 1], 'yo')

#PCA
hw_digits = np.loadtxt('/Volumes/UBUNTU-KYLI/zip.train')
num_3 = hw_digits[hw_digits[:, 0] == 3, 1:]
plt.imshow(num_3[1, :].reshape(16, 16))
import numpy.linalg as la
sigma = np.cov(num_3.T)
v,u = la.eig(sigma)
plt.imshow(u[:, 0].reshape(16, 16))

mean_3 = np.mean(num_3, axis = 0)
plt.imshow(mean_3.reshape(16, 16))

xi1 = (num_3 - mean_3).dot(v[:, 0]) #主成分得分，第一主成分得分对应手写“3”的胖瘦特征
indx = np.argsort()
plt.imshow(num_3[indx[1], :].reshape(16, 16))

#主成分重构，平滑的方法之一
rec = mean_3 + xi1.reshape(658, 1).dot(v[:, 0].reshape(1, 256))
