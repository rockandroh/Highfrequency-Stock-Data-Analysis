#%% import
import pandas as pd
import numpy as np
import time
import os
import warnings
import pickle
import matplotlib.pyplot as plt

from sklearn.linear_model import LinearRegression
from sklearn.linear_model import Ridge
from sklearn.linear_model import Lasso
from sklearn.ensemble import RandomForestRegressor

from sklearn.model_selection import GridSearchCV 
from sklearn.pipeline import Pipeline

from sklearn.feature_selection import SelectFromModel
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import f_regression #F-value between label/feature for regression tasks.

from statsmodels.tsa.api import VAR
from xgboost import XGBRegressor
pd.options.mode.chained_assignment = None  # default='warn'
warnings.simplefilter(action='ignore', category=FutureWarning)

#%%
def test_emh(data, target, prediction_horizon, test_length):
    y_true = data[target].shift(-prediction_horizon).fillna(method="ffill").values # lag1
    pred_current = data[target].values
    return y_true[-test_length:], pred_current[-test_length:]

#%% Get Data Split
"""
Split the Window

@raw_data: data
@lookback: number of data points for the analysis
@pred_horizon: prediction horizon = 1
@test_length: total test window (i.e., for last 2730 datapoints)

@description
return splitted trains, tests data set in a list
"""

def get_data_split(data_ready, lookback, test_length):
    
    data = data_ready.iloc[-test_length-lookback:,:]
    length = data.shape[0]
    train_starts = np.arange(0, length - lookback,1)
    # to make the train_starts and test_starts have the same number of elements
    test_starts = np.arange(lookback, length + 1, 1)[:len(train_starts)] 
    trains = [data.iloc[s:s+lookback] for s in train_starts]
    tests = [data.iloc[s:s+1] for s in test_starts]
    print('Split Done. Train Start')
    print('training set len:', len(trains))
    print('test set len:', len(tests))
    return trains, tests

#%% Get Diff variable with lags
def get_lag_diff(raw_data, order, target, exo=True):
    p = order - 1 
    X = raw_data.copy()
    # Generate lag variables 
    composite_col = X.columns[X.columns != target]
    X['y_pch'] = X[target].diff()
    X['y_pch_true'] = X['y_pch'].shift(-1)

    if exo:
        X[composite_col + '_pch'] = X[composite_col].diff()
        # make lag p variables and drop
        for i in range(0, p):
            X['y_pch_' + str(i+1)] = X['y_pch'].shift(i+1)
            X[composite_col + ('_' + str(i+1))] = X[composite_col + '_pch'].shift(i+1)      
    else:
        for i in range(0, p):
            X['y_pch_' + str(i+1)] = X['y_pch'].shift(i+1)

    # drop price series
    # don't drop target because we need to compare later
    X.drop(columns=composite_col, inplace=True) 
    X.dropna(inplace=True)
    print('Lag %d Diff Generated' %order)
    return X

#%% get_AR_moving_pred

"""
We want to predict the difference of price series and return it back to price series.
Here, we focus on the prediction from the linear model.
@raw_data: contain "price" series

get_AR_moving_pred
is by default order 1
"""

def get_AR_moving_pred(raw_data, order, test_length, target, exo=True):
    X = raw_data.copy()

    # generate diff and its lag variables
    data = get_lag_diff(X, 1, target)

    # dict for models    
    params0 = {'fit_intercept': [True, False]}
    params1 = {'alpha':[0.01, 0.05, 0.1, 0.3, 0.5, 1, 5, 10, 20, 50, 100, 200]}
    params2 = {'n_estimators':[10,100,1000],
               'max_depth':[5,8,15,25],
               'min_samples_leaf' :[1,2,5,10],
               'max_leaf_nodes' :[2,5,10]
            }
    models = {
            'RFR': GridSearchCV(RandomForestRegressor(), 
                               param_grid=params2, iid='deprecated', cv=5),
            'OLS': GridSearchCV(LinearRegression(), 
                               param_grid=params0, iid='deprecated', cv=3),
            'LR': GridSearchCV(Lasso(), 
                               param_grid=params1, iid='deprecated', cv=5),
            'RR': GridSearchCV(Ridge(), 
                               param_grid=params1, iid='deprecated', cv=5),
                        
            }

    pred_result = {}
    df_err_model = pd.DataFrame([])

    for m in models:

        for n in range(30, 10001, 3000):

            # split the data with different lookback n
            trains, tests = get_data_split(data_ready = data, lookback = n, test_length = test_length)
            print('Lookback : %d' %n)
            min_score = 0

            pred_price = np.array([])
            pred_price_best = np.array([])
            true_price = np.array([])

            for i in range(len(trains)):
                
                iteration_start = time.monotonic()
                X_train = trains[i].drop(columns=['y_pch_true', target])
                Y_train = trains[i]['y_pch_true']

                model_fit = models[m].fit(X_train, Y_train)
                selected_params = model_fit.best_params_
                best_model = model_fit.best_estimator_
                print('best model selected')
                X_new = SelectFromModel(estimator = best_model, threshold='median').fit(X_train,Y_train)
                print('best input selected')
                best_model.fit(X_new.transform(X_train),Y_train)
                selected_col = X_train.columns[X_new.get_support()]
                pred_diff = best_model.predict(tests[i].drop(columns=['y_pch_true', target])[selected_col])
                
                # recover price series after feature selection
                true_price = np.append(true_price, trains[i][target].iloc[-1:].values+ trains[i]['y_pch_true'].iloc[-1:].values)
                pred_price = np.append(pred_price, trains[i][target].iloc[-1:].values + pred_diff)
                
                iteration_end = time.monotonic()
                if(i % 50 == 1):
                    print('{:.2f}%'.format(i/(len(trains)+1)*100))
                    print("Iter time of %s: " %m, iteration_end - iteration_start)
                    print(selected_params)

            err_metric = evaluate(true_price, pred_price)
            score = err_metric['mda'] / err_metric['rmse']
            if min_score < score:
                min_score = score
                print("Saving...")
                fname = str(m)+'_best.txt'
                f = [true_price, pred_price, n]
                with open(fname,"wb") as fp:
                    pickle.dump(f,fp)
                pred_price_best = pred_price
            
            df = pd.DataFrame(err_metric.items())
            df = df.transpose()
            df.columns = df.iloc[0]
            df = df.drop(df.index[[0]]).astype(float) # change to float to use round(3)
            df['lookback']=str(n)
            df['model']=str(m)
            print(df)
            #df.columns = [str(col) + '_' + str(m) for col in df.columns]
            df_err_model = df_err_model.append(df, ignore_index = True)
            print(df_err_model)
        pred_result[m] = pred_price_best

    return  true_price, pred_result, df_err_model
#%% test

price_trues, preds_ls, df_err_model  = get_AR_moving_pred(df,1,500,'NDX')

with open('LR_best.txt', "rb") as fp:
    LR_best = pickle.load(fp)

#%%
def get_VAR_moving_pred(raw_data, lookback, test_length, target):
    X = raw_data.copy()
    #p = order - 1, here order is always 1
    price_var_preds = np.array([])
    price_trues = np.array([])

    colnames = X.columns
    X[colnames + '_pch'] = X[colnames].diff()
    X['y_pch_true']=X[str(target)+'_pch'].shift(-1)

    X.drop(columns=colnames[colnames != target], inplace=True)
    X.dropna(inplace=True)

    trains, tests = data_split(X, lookback, test_length)
    
    colnames = trains[0].columns[(trains[0].columns !='y_pch_true') & (trains[0].columns != target)] 
    # target is used only for recovering the price
    # we dont want y_pch_true in VAR

    for i in range(len(trains)):
        if(i % 100 == 0):
            print('{:.2f}%'.format(i/(len(trains)+1)*100))
        var = VAR(trains[i][colnames].reset_index(drop=True))
        var_fitted = var.fit(1) # order 1
        y_pred = var_fitted.forecast(y=tests[i][colnames].values, steps=1)
        price_var_preds = np.append(price_var_preds, trains[i][target].iloc[-1:] + y_pred.flatten()[-1])
        price_trues = np.append(price_trues, tests[i][target].values)
        
    return price_trues, price_var_preds

#%%
def get_XGB_moving_pred(raw_data, order, lookback, test_length, target):
    X = raw_data.copy()
    p = order - 1
    price_preds = np.array([])
    price_preds_new = np.array([])
    price_trues = np.array([])

    composite_col = X.columns[X.columns != target]
    X['y_pch'] = X[target].diff()
    X['y_pch_true'] = X['y_pch'].shift(-1)
    X[composite_col + '_pch'] = X[composite_col].diff()

    # Generate lag variables
    for i in range(0, p):
        X['y_pch_' + str(i+1)] = X['y_pch'].shift(i+1)
        X[composite_col + ('_' + str(i+1))] = X[composite_col + '_pch'].shift(i+1)
    
    # drop price series / leave only diff sereis
    # don't drop target because we need it in estimation
    X.drop(columns=composite_col, inplace=True)
    X.dropna(inplace=True)
    
    # split data
    trains, tests = data_split(X, lookback, test_length)

    # fit the data

    # percentage of features used per tree. High value can lead to overfitting.
    xgb=  XGBRegressor(base_score=0.5, colsample_bytree=0.8, gamma=0,
           learning_rate=0.1, max_delta_step=0, max_depth=6,
           min_child_weight=100, missing=None, n_estimators=100, nthread=-1, objective='reg:squarederror', reg_alpha=0, seed=1234, subsample=0.8)
    xgb_new=  XGBRegressor(base_score=0.5, colsample_bytree=0.8, gamma=0,
           learning_rate=0.1, max_delta_step=0, max_depth=6,
           min_child_weight=100, missing=None, n_estimators=100, nthread=-1, objective='reg:squarederror', reg_alpha=0, seed=1234, subsample=0.8)
    
    n_exo = trains[0].drop(columns=['y_pch_true',target]).shape[1]
    score = np.zeros(n_exo).reshape(-1,1)

    for i in range(len(trains)):
        
        iteration_start = time.monotonic()
        X = trains[i].drop(columns=['y_pch_true',target])
        y = trains[i]['y_pch_true']

        # Fit
        xgb.fit(X,y)
        X_xgb = SelectFromModel(estimator = xgb_new, threshold='median').fit(X,y)
        xgb_new.fit(X_xgb.transform(X),y)
        xgb_col = X.columns[X_xgb.get_support()]

        score_each = xgb.feature_importances_
        y_pred = xgb.predict(tests[i].drop(columns=['y_pch_true',target]))
        y_pred_new = xgb_new.predict(tests[i].drop(columns=['y_pch_true',target])[xgb_col].values)
        price_preds = np.append(price_preds, trains[i][target].iloc[-1:] + y_pred)
        price_preds_new = np.append(price_preds_new, trains[i][target].iloc[-1:] + y_pred_new)
        price_trues = np.append(price_trues, tests[i][target].values)
        score = np.hstack((score,score_each.reshape(-1,1))) # concatenate over second axis
        iteration_end = time.monotonic()
    
        if(i % 100 == 0):
            print('{:.2f}%'.format(i/(len(trains)+1)*100))
            print("Iter time: ", iteration_end - iteration_start)

    score = score[:,1:] # delete zeros

    return price_trues, price_preds, price_preds_new, score


#%%
def get_RFR_moving_pred(raw_data,order, lookback, test_length, target):

    X = raw_data.copy()
    p = order - 1
    price_preds = np.array([])
    price_preds_new = np.array([])
    price_trues = np.array([])

    composite_col = X.columns[X.columns != target]
    X['y_pch'] = X[target].diff()
    X['y_pch_true'] = X['y_pch'].shift(-1)
    X[composite_col + '_pch'] = X[composite_col].diff()

    # Generate lag variables
    for i in range(0, p):
        X['y_pch_' + str(i+1)] = X['y_pch'].shift(i+1)
        X[composite_col + ('_' + str(i+1))] = X[composite_col + '_pch'].shift(i+1)

    # drop price series / leave only diff sereis
    # don't drop target because we need it in estimation
    X.drop(columns=composite_col, inplace=True)
    X.dropna(inplace=True)
    
    # split data
    trains, tests = data_split(X, lookback, test_length)

    n_exo = trains[0].drop(columns=['y_pch_true',target]).shape[1]
    score = np.zeros(n_exo).reshape(-1,1)

    rfr = RandomForestRegressor(n_estimators=100, criterion='mse', max_features='auto', random_state=1234, min_samples_leaf=0.05, n_jobs=-1)
    rfr_new = RandomForestRegressor(n_estimators=100, criterion='mse', max_features='auto', random_state=1234, min_samples_leaf=0.05, n_jobs=-1)
    
    for i in range(len(trains)):
            
            iteration_start = time.monotonic()
            X = trains[i].drop(columns=['y_pch_true',target])
            y = trains[i]['y_pch_true']
            
            # fit
            rfr.fit(X, y)
            score_each = rfr.feature_importances_
            col = X.columns[score_each.cumsum() / score_each.sum() <= .9]
            rfr.fit(X[col], y)

            X_rfr = SelectFromModel(estimator = rfr_new, threshold='median').fit(X,y)
            rfr_new.fit(X_rfr.transform(X),y)
            rfr_col = X.columns[X_rfr.get_support()]

            y_pred = rfr.predict(tests[i].drop(columns=['y_pch_true',target])[col])
            y_pred_new = rfr_new.predict(tests[i].drop(columns=['y_pch_true',target])[rfr_col])

            price_preds = np.append(price_preds, trains[i][target].iloc[-1:] + y_pred)
            price_preds_new = np.append(price_preds_new, trains[i][target].iloc[-1:] + y_pred_new)
            price_trues = np.append(price_trues, tests[i][target].values)
            score = np.hstack((score, score_each.reshape(-1,1))) # concatenate over second axis
            iteration_end = time.monotonic()

            if(i % 100 == 0):
                print('{:.2f}%'.format(i/(len(trains)+1)*100))
                print("Iter time: ", iteration_end - iteration_start)

    score = score[:,1:] # delete zeros
    return price_trues, price_preds, price_preds_new, score


import seaborn as sns
# full data set with multicollinearity
plt.figure(figsize=(15,8))
sns.heatmap(X.corr().abs().round(2)>=0.60)

#%%
from sklearn.decomposition import PCA
from sklearn.cross_decomposition import PLSRegression, PLSSVD

def get_DIM_moving_pred(raw_data, order, lookback, test_length, target, exo=True):
    X = raw_data.copy()
    p = order - 1
    price_pca_preds1 = np.array([])
    price_pca_preds2 = np.array([])
    
    price_pls_preds1 = np.array([])
    price_pls_preds2 = np.array([])
    
    price_trues = np.array([])

    # Generate lag variables 
    composite_col = X.columns[X.columns != target]
    X['y_pch'] = X[target].diff()
    X['y_pch_true'] = X['y_pch'].shift(-1)

    if exo:
        X[composite_col + '_pch'] = X[composite_col].diff()
        # make lag p variables and drop
        for i in range(0, p):
            X['y_pch_' + str(i+1)] = X['y_pch'].shift(i+1)
            X[composite_col + ('_' + str(i+1))] = X[composite_col + '_pch'].shift(i+1)      
    else:
        for i in range(0, p):
            X['y_pch_' + str(i+1)] = X['y_pch'].shift(i+1)

    # drop price series
    # don't drop target because we need to compare later
    X.drop(columns=composite_col, inplace=True) 
    X.dropna(inplace=True)

    # split the data
    trains, tests = get_data_split(X, lookback, test_length)

    for i in range(len(trains)):
        
        iteration_start = time.monotonic()
        X = trains[i].drop(columns=['y_pch_true', target])
        X_mean = np.mean(X, axis=0)
        X = X-X_mean # broadcassting to make it centered
        X_test = tests[i].drop(columns=['y_pch_true', target]).values - X_mean[np.newaxis,:] # centered for test set
        Y = trains[i]['y_pch_true']
        
        price_pca_ls = []
        price_pls_ls = []
        
        # pca with different # of PC
        for j in [2,3,20,30]:

            price_pca_preds = np.array([])
            pca = PCA(n_components=j)
            X_pca = pca.fit(X)
            ols = LinearRegression()
            ols.fit(X_pca.fit_transform(X), Y)
            pca_pred = ols.predict(np.matmul(X_test,
                                np.transpose(X_pca.components_)))
            price_pca_preds = np.append(price_pca_preds, trains[i][target].iloc[-1:].values + pca_pred)
            price_pca_ls.append(price_pca_preds)

            price_pls_preds = np.array([])
            pls = PLSRegression(n_components=j)
            pls.fit(X, Y)
            pls_pred = pls.predict(X_test) # we dont have to do: np.matmul(X_test, X_pls.x_loadings_)
            price_pls_preds = np.append(price_pls_preds, trains[i][target].iloc[-1:].values + pls_pred)
            price_pls_ls.append(price_pls_preds)
            
        price_pca_preds1 = np.append(price_pca_preds1, price_pca_ls[0])
        price_pca_preds2 = np.append(price_pca_preds2, price_pca_ls[1])

        price_pls_preds1 = np.append(price_pls_preds1, price_pls_ls[0])
        price_pls_preds2 = np.append(price_pls_preds2, price_pls_ls[1])


        price_trues = np.append(price_trues, trains[i][target].iloc[-1:].values+ trains[i]['y_pch_true'].iloc[-1:].values)
        
        iteration_end = time.monotonic()
        if(i % 100 == 0):
            print('{:.2f}%'.format(i/(len(trains)+1)*100))
            print("Iter time: ", iteration_end - iteration_start)

    price_pca_ls = [price_pca_preds1,price_pca_preds2]
    price_pls_ls = [price_pls_preds1,price_pls_preds2]
    return  price_trues, price_pca_ls, price_pls_ls

price_trues, price_pca_ls, price_pls_ls = get_DIM_moving_pred(df,1,6000,2730,'NDX')

# %%
def get_DIM_moving_pred_mod(raw_data, order, lookback, test_length, target, exo=True):
    X = raw_data.copy()
    p = order - 1
    # Generate lag variables 
    composite_col = X.columns[X.columns != target]
    X['y_pch'] = X[target].diff()
    X['y_pch_true'] = X['y_pch'].shift(-1)

    if exo:
        X[composite_col + '_pch'] = X[composite_col].diff()
        # make lag p variables and drop
        for i in range(0, p):
            X['y_pch_' + str(i+1)] = X['y_pch'].shift(i+1)
            X[composite_col + ('_' + str(i+1))] = X[composite_col + '_pch'].shift(i+1)      
    else:
        for i in range(0, p):
            X['y_pch_' + str(i+1)] = X['y_pch'].shift(i+1)

    # drop price series
    # don't drop target because we need to compare later
    X.drop(columns=composite_col, inplace=True) 
    X.dropna(inplace=True)

    # split the data
    trains, tests = get_data_split(X, lookback, test_length)
    

    pred_price = np.array([])
    true_price = np.array([])
    pca_num = np.array([])

    for i in range(len(trains)):
        
        iteration_start = time.monotonic()
        X = trains[i].drop(columns=['y_pch_true', target])
        X_mean = np.mean(X, axis=0)
        X = X-X_mean # broadcassting to make it centered
        X_test = tests[i].drop(columns=['y_pch_true', target]).values - X_mean[np.newaxis,:] # centered for test set
        Y = trains[i]['y_pch_true']
        
        # select best n_component for each train set
        pca_min_score = 100000000
        pca_num = 0
        ols = LinearRegression()

        # pca with different # of PC
        for j in [2,3,20,30]:

            price_pca_preds = np.array([])
            pca = PCA(n_components=j)
            X_pca = pca.fit(X)
            
            ols.fit(X_pca.fit_transform(X), Y)
            pca_pred = ols.predict(np.matmul(X_test,
                                np.transpose(X_pca.components_)))
            price_pca_ls.append(price_pca_preds)
            pca_score = mse(trains[i]['y_pch_true'].iloc[-1:].values, pca_pred)
            
            if pca_score < pca_min_score:
                pca_min_score = pca_score
                pred_diff = pca_pred
                best_j = j
        pca_num = np.append(pca_num, best_j)

        pred_price = np.append(pred_price, trains[i][target].iloc[-1:].values + pred_diff)        
        true_price = np.append(price_trues, trains[i][target].iloc[-1:].values+ trains[i]['y_pch_true'].iloc[-1:].values)
        
        iteration_end = time.monotonic()
        if(i % 100 == 0):
            print('{:.2f}%'.format(i/(len(trains)+1)*100))
            print("Iter time: ", iteration_end - iteration_start)


    return  true_price, pred_price, pca_num

# %%
true_price, pred_price, pca_num = get_DIM_moving_pred_mod(df,1,6000,2730,'NDX')
# %%
