# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as pl

# Test data
n = 50
Xtest = np.linspace(-5, 5, n).reshape(-1,1)

# Define the kernel function
def kernel(a, b, param):
    #returns kernal matrix k(a e b)
    sqdist = np.sum(a**2,1).reshape(-1,1) + np.sum(b**2,1) - 2*np.dot(a, b.T)
    return np.exp(-.5 * (1/param) * sqdist)

param = 0.1
K_ss = kernel(Xtest, Xtest, param)
# Noiseless training data
Xtrain = np.array([-4, -3, -2, -1, 1]).reshape(5,1)
ytrain = np.sin(Xtrain)

# Apply the kernel function to our training points
K = kernel(Xtrain, Xtrain, param)
L = np.linalg.cholesky(K + 0.00005*np.eye(len(Xtrain)))

# Compute the mean at our test points.
# uses standard computation of conditional mean and covariances
# mu = mu(X*) + K*.T Inv(K)(y_train-mu(x_train))
# there K* is K(xtrain,x_predict), K is K(xtrain,xtrain)
# wih L as the Cholesky decomp of K (= L*L.T). Simple manipulation give the solution at solution to operations given below.
K_s = kernel(Xtrain, Xtest, param)
Lk = np.linalg.solve(L, K_s)
mu = np.dot(Lk.T, np.linalg.solve(L, ytrain)).reshape((n,))


# Compute the standard deviation so we can plot it
# Again this is an implementation of the conditional distribution of predictive covariance.
s2 = np.diag(K_ss) - np.sum(Lk**2, axis=0)
stdv = np.sqrt(s2)
# Draw samples from the posterior at our test points.
L = np.linalg.cholesky(K_ss + 1e-6*np.eye(n) - np.dot(Lk.T, Lk))
# If x is a vector with each component is N(0,1) then var of Lx is K_ss
# < Lx (Lx).T) > = <Lx(x.T)(L.T)> by linearlity of expecations = L<x(x.T)>L.T=L(L.T)=K
f_post = mu.reshape(-1,1) + np.dot(L, np.random.normal(size=(n,53)))

pl.plot(Xtrain, ytrain, 'bs', ms=8)
pl.plot(Xtest, f_post)
pl.gca().fill_between(Xtest.flat, mu-2*stdv, mu+2*stdv, color="#dddddd")
pl.plot(Xtest, mu, 'r--', lw=2)
pl.axis([-5, 5, -3, 3])
pl.title('Three samples from the GP posterior')
pl.show()