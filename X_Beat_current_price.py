# -*- coding: utf-8 -*-
"""
Created on Wed Sep 25 16:17:20 2019

@author: User
"""

import  numpy as np
import pandas as pd
from sklearn.metrics import mean_squared_error, mean_absolute_error
import matplotlib.pyplot as plt


#%%

np.set_printoptions(suppress=True)
pd.set_option('display.max_rows',1000)
pd.set_option('display.max_columns',1000)

#%% What is MSE just to take the previous close price?

data = pd.read_csv('C:/Users/User/Dropbox/Master_Thesis/Data/data_ready.csv')
nasdaq = pd.read_csv('C:/Users/User/Dropbox/Master_Thesis/Data/nasdaq/nasdaq100_padding.csv')
data.head()
nasdaq.head()

batch_size = 64
timesteps = 90 # hours
n_timeseries = data.shape[1]
train_length = 9029
val_length = 1200
test_length = 1200
target = 'close'
prediction_horizon = 1

target = data['close'].shift(-prediction_horizon).fillna(method='ffill').values
y = data['close'].values
y_open = data['open'].values
y_high = data['high'].values
y_low = data['low'].values
y_mid = (data['high'].values + data['low'].values)/2
target[:10]
y[:10]

len(target) - np.count_nonzero(y-target)

target_train = target[:train_length]
target_val = target[train_length:train_length+val_length]
target_test = target[-val_length:]
y_close = y[-val_length:]
y_open = y_open[-val_length:]
y_high = y_high[-val_length:]
y_low = y_low[-val_length:]
y_mid = y_mid[-val_length:]


plt.plot(y)
plt.plot(target_test)
plt.show()

len(y)
len(target_test)

y[1119]
target_test[1119], target_test[1118]

target_train_max = target_train.max(axis=0)
target_train_min = target_train.min(axis=0)

preds = y_close*(target_train_max - target_train_min) + target_train_min
preds_high = y_high*(target_train_max - target_train_min) + target_train_min
preds_low = y_low*(target_train_max - target_train_min) + target_train_min
preds_open = y_open*(target_train_max - target_train_min) + target_train_min
preds_mid = y_mid*(target_train_max - target_train_min) + target_train_min
preds = y_close*(target_train_max - target_train_min) + target_train_min

true = target_test*(target_train_max - target_train_min) + target_train_min

mse_close = mean_squared_error(true, preds)
mae_close = mean_absolute_error(true, preds)
mse_high = mean_squared_error(true, preds_high)
mse_low = mean_squared_error(true, preds_low)
mse_open = mean_squared_error(true, preds_open)
mse_mid = mean_squared_error(true,preds_mid)

#%% what about 1 min nasdaq?

timesteps = 90 # hours
n_timeseries = data.shape[1]
train_length = 35100
val_length = 2730
test_length = 2730
target = 'NDX'
prediction_horizon = 1

target = nasdaq['NDX'].shift(-prediction_horizon).fillna(method='ffill').values
y = nasdaq['NDX'].values

target[:10]
y[:10]

len(target) - np.count_nonzero(y-target)

target_train = target[:train_length]
target_val = target[train_length:train_length+val_length]
target_test = target[-val_length:]
y_ndx = y[-val_length:]


plt.plot(y_ndx)
plt.plot(target_test)
plt.show()

plt.plot(preds)
plt.plot(true)
plt.show()

len(y_ndx)
len(target_test)

target_train.max()

target_train_max = target_train.max(axis=0)
target_train_min = target_train.min(axis=0)

preds = y_ndx
true = target_test

mse_close = mean_squared_error(true, preds)
mae_close = mean_absolute_error(true, preds)
mse_high = mean_squared_error(true, preds_high)
mse_low = mean_squared_error(true, preds_low)
mse_open = mean_squared_error(true, preds_open)
mse_mid = mean_squared_error(true,preds_mid)

#%%



