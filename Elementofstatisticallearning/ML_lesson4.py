# -*- coding: utf-8 -*-
"""
Created on Mon Mar 30 18:45:50 2015

@author: wangjinling
"""
import numpy as np
from numpy import linalg as la

#group effection
x1=np.random.randn(1000).reshape((1000,1))
err=np.random.randn(1000).reshape((1000,1))*0.1
y=2*x1+3+err
X=np.hstack((np.ones((1000, 1)),x1,x1,x1))

lam=0.001#否则奇异矩阵，没有逆矩阵
la.inv(X.T.dot(X)+np.eye(4)*lam).dot(X.T).dot(y)

u,d,v = la.svd(X) #v最后两列相同