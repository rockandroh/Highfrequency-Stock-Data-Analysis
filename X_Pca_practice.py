# -*- coding: utf-8 -*-
"""
Created on Wed Jul 29 21:36:09 2020

@author: Roh
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

from sklearn import decomposition
from sklearn import datasets
from sklearn.preprocessing import scale
from sklearn.decomposition import PCA
from sklearn.decomposition import KernelPCA
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import GridSearchCV 
iris = datasets.load_iris()
params_ols = {'fit_intercept': [True, False]}
df = iris.data
df.shape
df = scale(df)
df.shape
X = df[0:149,0:3]
X2 = df[149,0:3]
X2.reshape(-1,1)
X.shape
Y = df[0:149,3]

pca_fit = PCA(n_components=2).fit(X)
pca_fit.components_
np.matmul(X2, np.transpose(pca_fit.components_))
pca_fit.transform(X2.reshape(1,-1))

a = np.matmul(X, np.transpose(pca_fit.components_))

kpca_fit = KernelPCA(n_components=2, kernel='rbf', fit_inverse_transform=True).fit(X)

kpca_fit.transform(X2.reshape(-1,1))

kpca_fit.X_transformed_fit_
kpca_fit.fit_transform(X)

kpca_fit.dual_coef_

model_fit = GridSearchCV(LinearRegression(),param_grid=params_ols, iid='deprecated', cv=5).fit(b,Y)
slected_params = model_fit.best_params_
best_model = model_fit.best_estimator_
b.shape
pred_diff_pca = best_model.predict(b)
pred_diff_pca2 = best_model.predict(a)
pred_diff_pca2.shape
pred_diff_pca.reshape(1,1).shape

kpca_score = mse(pred_diff_pca, pred_diff_pca)
X.shape
X_new = np.linspace(1,100, 150)
X_new.shape
