#%% Evaluation Cell

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

EPSILON = 1e-10

#%%

def _error(actual: np.ndarray, predicted: np.ndarray):
    """ Simple error """
    return actual - predicted

def _percentage_error(actual: np.ndarray, predicted: np.ndarray):
    """
    Percentage error

    Note: result is NOT multiplied by 100
    """
    return _error(actual, predicted) / (actual + EPSILON)

def _naive_forecasting(actual: np.ndarray, seasonality: int = 1):
    """ Naive forecasting method which just repeats previous samples """
    return actual[:-seasonality]


def _relative_error(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Relative Error """
    if benchmark is None or isinstance(benchmark, int):
        # If no benchmark prediction provided - use naive forecasting
        if not isinstance(benchmark, int):
            seasonality = 1
        else:
            seasonality = benchmark
        return _error(actual[seasonality:], predicted[seasonality:]) /\
               (_error(actual[seasonality:], _naive_forecasting(actual, seasonality)) + EPSILON)

    return _error(actual, predicted) / (_error(actual, benchmark) + EPSILON)


def _bounded_relative_error(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Bounded Relative Error """
    if benchmark is None or isinstance(benchmark, int):
        # If no benchmark prediction provided - use naive forecasting
        if not isinstance(benchmark, int):
            seasonality = 1
        else:
            seasonality = benchmark

        abs_err = np.abs(_error(actual[seasonality:], predicted[seasonality:]))
        abs_err_bench = np.abs(_error(actual[seasonality:], _naive_forecasting(actual, seasonality)))
    else:
        abs_err = np.abs(_error(actual, predicted))
        abs_err_bench = np.abs(_error(actual, benchmark))

    return abs_err / (abs_err + abs_err_bench + EPSILON)


def _geometric_mean(a, axis=0, dtype=None):
    """ Geometric mean """
    if not isinstance(a, np.ndarray):  # if not an ndarray object attempt to convert it
        log_a = np.log(np.array(a, dtype=dtype))
    elif dtype:  # Must change the default dtype allowing array type
        if isinstance(a, np.ma.MaskedArray):
            log_a = np.log(np.ma.asarray(a, dtype=dtype))
        else:
            log_a = np.log(np.asarray(a, dtype=dtype))
    else:
        log_a = np.log(a)
    return np.exp(log_a.mean(axis=axis))


def mse(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Squared Error """
    return np.mean(np.square(_error(actual, predicted)))


def rmse(actual: np.ndarray, predicted: np.ndarray):
    """ Root Mean Squared Error """
    return np.sqrt(mse(actual, predicted))


def nrmse(actual: np.ndarray, predicted: np.ndarray):
    """ Normalized Root Mean Squared Error """
    return rmse(actual, predicted) / (actual.max() - actual.min())


def me(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Error """
    return np.mean(_error(actual, predicted))


def mae(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Absolute Error """
    return np.mean(np.abs(_error(actual, predicted)))

mad = mae  # Mean Absolute Deviation (it is the same as MAE)

def gmae(actual: np.ndarray, predicted: np.ndarray):
    """ Geometric Mean Absolute Error """
    return _geometric_mean(np.abs(_error(actual, predicted)))


def mdae(actual: np.ndarray, predicted: np.ndarray):
    """ Median Absolute Error """
    return np.median(np.abs(_error(actual, predicted)))


def mpe(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Percentage Error """
    return np.mean(_percentage_error(actual, predicted))


def mape(actual: np.ndarray, predicted: np.ndarray):
    """
    Mean Absolute Percentage Error

    Properties:
        + Easy to interpret
        + Scale independent
        - Biased, not symmetric
        - Undefined when actual[t] == 0

    Note: result is NOT multiplied by 100
    """
    return np.mean(np.abs(_percentage_error(actual, predicted)))*100


def mdape(actual: np.ndarray, predicted: np.ndarray):
    """
    Median Absolute Percentage Error

    Note: result is NOT multiplied by 100
    """
    return np.median(np.abs(_percentage_error(actual, predicted)))


def smape(actual: np.ndarray, predicted: np.ndarray):
    """
    Symmetric Mean Absolute Percentage Error

    Note: result is NOT multiplied by 100
    """
    return np.mean(2.0 * np.abs(actual - predicted) / ((np.abs(actual) + np.abs(predicted)) + EPSILON))


def smdape(actual: np.ndarray, predicted: np.ndarray):
    """
    Symmetric Median Absolute Percentage Error

    Note: result is NOT multiplied by 100
    """
    return np.median(2.0 * np.abs(actual - predicted) / ((np.abs(actual) + np.abs(predicted)) + EPSILON))


def maape(actual: np.ndarray, predicted: np.ndarray):
    """
    Mean Arctangent Absolute Percentage Error

    Note: result is NOT multiplied by 100
    """
    return np.mean(np.arctan(np.abs((actual - predicted) / (actual + EPSILON))))


def mase(actual: np.ndarray, predicted: np.ndarray, seasonality: int = 1):
    """
    Mean Absolute Scaled Error

    Baseline (benchmark) is computed with naive forecasting (shifted by @seasonality)
    """
    return mae(actual, predicted) / mae(actual[seasonality:], _naive_forecasting(actual, seasonality))


def std_ae(actual: np.ndarray, predicted: np.ndarray):
    """ Normalized Absolute Error """
    __mae = mae(actual, predicted)
    return np.sqrt(np.sum(np.square(_error(actual, predicted) - __mae))/(len(actual) - 1))


def std_ape(actual: np.ndarray, predicted: np.ndarray):
    """ Normalized Absolute Percentage Error """
    __mape = mape(actual, predicted)
    return np.sqrt(np.sum(np.square(_percentage_error(actual, predicted) - __mape))/(len(actual) - 1))


def rmspe(actual: np.ndarray, predicted: np.ndarray):
    """
    Root Mean Squared Percentage Error

    Note: result is NOT multiplied by 100
    """
    return np.sqrt(np.mean(np.square(_percentage_error(actual, predicted))))


def rmdspe(actual: np.ndarray, predicted: np.ndarray):
    """
    Root Median Squared Percentage Error

    Note: result is NOT multiplied by 100
    """
    return np.sqrt(np.median(np.square(_percentage_error(actual, predicted))))


def rmsse(actual: np.ndarray, predicted: np.ndarray, seasonality: int = 1):
    """ Root Mean Squared Scaled Error """
    q = np.abs(_error(actual, predicted)) / mae(actual[seasonality:], _naive_forecasting(actual, seasonality))
    return np.sqrt(np.mean(np.square(q)))


def inrse(actual: np.ndarray, predicted: np.ndarray):
    """ Integral Normalized Root Squared Error """
    return np.sqrt(np.sum(np.square(_error(actual, predicted))) / np.sum(np.square(actual - np.mean(actual))))


def rrse(actual: np.ndarray, predicted: np.ndarray):
    """ Root Relative Squared Error """
    return np.sqrt(np.sum(np.square(actual - predicted)) / np.sum(np.square(actual - np.mean(actual))))


def mre(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Mean Relative Error """
    return np.mean(_relative_error(actual, predicted, benchmark))


def rae(actual: np.ndarray, predicted: np.ndarray):
    """ Relative Absolute Error (aka Approximation Error) """
    return np.sum(np.abs(actual - predicted)) / (np.sum(np.abs(actual - np.mean(actual))) + EPSILON)


def mrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Mean Relative Absolute Error """
    return np.mean(np.abs(_relative_error(actual, predicted, benchmark)))


def mdrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Median Relative Absolute Error """
    return np.median(np.abs(_relative_error(actual, predicted, benchmark)))


def gmrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Geometric Mean Relative Absolute Error """
    return _geometric_mean(np.abs(_relative_error(actual, predicted, benchmark)))


def mbrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Mean Bounded Relative Absolute Error """
    return np.mean(_bounded_relative_error(actual, predicted, benchmark))


def umbrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Unscaled Mean Bounded Relative Absolute Error """
    __mbrae = mbrae(actual, predicted, benchmark)
    return __mbrae / (1 - __mbrae)

def da(actual: np.ndarray, predicted: np.ndarray):
    """ Directional Indicator """
    sign = (np.sign(actual[1:] - actual[:-1]) == np.sign(predicted[1:] - actual[:-1])).astype(int)
    sign[sign==0]=-1
    return sign

def mda(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Directional Accuracy """
    return np.mean((np.sign(actual[1:] - actual[:-1]) == np.sign(predicted[1:] - actual[:-1])).astype(int))

def mds(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Directional Score """
    mds = np.mean(abs(_error(actual,predicted)[1:])*da(actual,predicted))
    return mds

def mda_variant(actual: np.ndarray, predicted: np.ndarray):
    # Actual Up and Predict Up
    upup = (3*(np.ones(len(actual)-1)) == (2*np.sign(actual[1:] - actual[:-1]) + 
                                                    np.sign(predicted[1:] - actual[:-1])))   

    # Actual Up and Predict Down
    updown = (1*(np.ones(len(actual)-1)) == (2*np.sign(actual[1:] - actual[:-1]) + 
                                                    np.sign(predicted[1:] - actual[:-1])))
    # Actual Down and Predict Up
    downup = (-1*(np.ones(len(actual)-1)) == (2*np.sign(actual[1:] - actual[:-1]) + 
                                                    np.sign(predicted[1:] - actual[:-1])))

    # Actual Down and Predict Down
    downdown = (-3*(np.ones(len(actual)-1)) == (2*np.sign(actual[1:] - actual[:-1]) + 
                                                    np.sign(predicted[1:] - actual[:-1])))

    MPDA = np.sum(upup.astype(int))/np.sum(np.append(upup,updown).astype(int))
    MNDA = np.sum(downdown.astype(int))/np.sum(np.append(downdown,downup).astype(int))
    MPP = np.sum(upup.astype(int))/np.sum(np.append(upup,downup).astype(int))
    MNP = np.sum(downdown.astype(int))/np.sum(np.append(downdown,updown).astype(int))
    fscore = (np.sum(upup.astype(int))+np.sum(downdown.astype(int)))/(np.sum(np.append(upup,updown).astype(int))+np.sum(np.append(downdown,downup).astype(int)))

    return [MPDA, MNDA, MPP, MNP, fscore]

def mpda(actual: np.ndarray, predicted: np.ndarray):
    return mda_variant(actual, predicted)[0]

def mnda(actual: np.ndarray, predicted: np.ndarray):
    return mda_variant(actual, predicted)[1]
    
def mpp(actual: np.ndarray, predicted: np.ndarray):
    return mda_variant(actual, predicted)[2]

def mnp(actual: np.ndarray, predicted: np.ndarray):
    return mda_variant(actual, predicted)[3]

def fscore(actual: np.ndarray, predicted: np.ndarray):
    return mda_variant(actual, predicted)[4]

def autocovariance(Xi, N, k, Xs):
        autoCov = 0
        T = float(N)
        for i in np.arange(0, N-k):
              autoCov += ((Xi[i+k])-Xs)*(Xi[i]-Xs)
        return (1/(T))*autoCov

def simple_backtesting(actual: np.ndarray, predicted: np.ndarray):
    ini_cap = 1
    predicted_direction = predicted[1:] - actual[:-1]
    signal = np.zeros(len(predicted_direction))
    signal[predicted_direction > 0] = 1 # expect price to rise
    signal[predicted_direction < 0] = -1 # expect price to fall
    ret = actual[1:]/actual[:-1] - 1
    pnl = ini_cap * np.cumprod(1 + ret*signal)
    profit = (pnl[-1]-1)*100
    mdd = np.min(ret*signal)*100
    SR = np.sqrt(252)*np.nanmean(np.diff(pnl, prepend=np.nan)/pnl) / np.nanstd(np.diff(pnl, prepend=np.nan)/pnl) #annualized
    return [profit, mdd, SR]

def pct_return(actual: np.ndarray, predicted: np.ndarray):
    return simple_backtesting(actual, predicted)[0]

def mdd(actual:np.ndarray, predicted: np.ndarray):
    return simple_backtesting(actual, predicted)[1]

def sr(actual:np.ndarray, predicted: np.ndarray):
    return simple_backtesting(actual, predicted)[2]

METRICS = {
    'mse': mse,
    'rmse': rmse,
    'nrmse': nrmse,
    'me': me,
    'mae': mae,
    'mad': mad,
    'gmae': gmae,
    'mdae': mdae,
    'mpe': mpe,
    'mape': mape,
    'mdape': mdape,
    'smape': smape,
    'smdape': smdape,
    'maape': maape,
    'mase': mase,
    'std_ae': std_ae,
    'std_ape': std_ape,
    'rmspe': rmspe,
    'rmdspe': rmdspe,
    'rmsse': rmsse,
    'inrse': inrse,
    'rrse': rrse,
    'mre': mre,
    'rae': rae,
    'mrae': mrae,
    'mdrae': mdrae,
    'gmrae': gmrae,
    'mbrae': mbrae,
    'umbrae': umbrae,
    'mda': mda,
    'mpda': mpda,
    'mnda': mnda,
    'mpp': mpp,
    'mnp': mnp,
    'mds': mds,
    'pct_r': pct_return,
    'mdd': mdd,
    'sr': sr
}


def evaluate(actual: np.ndarray, predicted: np.ndarray, metrics=('rmse', 'mape', 'mda', 'pct_r', 'mpda', 'mnda', 'mpp', 'mnp','mds','mdd','sr')):
    results = {}
    for name in metrics:
        try:
            results[name] = METRICS[name](actual, predicted)
        except Exception as err:
            results[name] = np.nan
            print('Unable to compute metric {0}: {1}'.format(name, err))
    return results

def evaluate_all(actual: np.ndarray, predicted: np.ndarray):
    return evaluate(actual, predicted, metrics=set(METRICS.keys()))


#%%
    

# Arithematic Return

def backtesting(actual, predicted, plot=True, title="title"):
    ini_cap = 1
    predicted_direction = predicted[1:] - actual[:-1]
    diff = actual[1:] - actual[:-1]
    signal = np.zeros(len(predicted_direction))
    signal[predicted_direction > 0] = 1 # expect price to rise
    signal[predicted_direction < 0] = -1 # expect price to fall
    ret = actual[1:]/actual[:-1] - 1
    ret_log = np.log(actual[1:])-np.log(actual[:-1])
    pnl = ini_cap * np.cumprod(1 + ret*signal)
    df = pd.DataFrame({'pnl': np.append(ini_cap,pnl),
                   'ret': np.append(ini_cap,np.cumprod(1+ret)),
                   'position': np.append(ini_cap,signal)
                  })
    mdd = np.min(ret*signal)*100
    mda_value = mda(actual, predicted) * 100
    acc_return = (pnl[-1]-1)*100
    # create figure and axis objects with subplots()
    fig,ax = plt.subplots(figsize=(25,10))
    # make a plot
    ax.plot(df['ret'], color='blue', label='return')
    ax.plot(df['pnl'], color='red', label='strategy')
    # set y-axis label
    ax.set_ylabel("Cumulative Return",fontsize=15)
    # set x-axis label
    ax.set_xlabel("Time(Minute)",fontsize=15)

    # twin object for two different y-axis on the sample plot
    ax2=ax.twinx()
    # make a plot with different y-axis using second axis object
    ax2.scatter(x=range(len(df['position'])),y=df['position'],c="green", s=5)
    ax2.set_ylabel("Position",fontsize=15)
    #ax.legend(loc='best', fontsize='x-large', frameon=True, facecolor='white', framealpha=1)
    ax.legend(bbox_to_anchor=(0.1, 0.9), fontsize='x-large', frameon=True)

    ax.text(0.95, 0.6, 'RMSE: %.4f' %rmse(actual, predicted),
        verticalalignment='center', horizontalalignment='right',
        transform=ax.transAxes,
        color='green', fontsize=16)
    ax.text(0.95, 0.55, 'MDA: %.3f' %mda_value + '%',
        verticalalignment='center', horizontalalignment='right',
        transform=ax.transAxes,
        color='green', fontsize=16)
    ax.text(0.95, 0.5, 'MDD: %.4f' %mdd + '%',
        verticalalignment='center', horizontalalignment='right',
        transform=ax.transAxes,
        color='green', fontsize=16)
    ax.text(0.95, 0.45, 'PnL: %.4f' %acc_return + '%',
        verticalalignment='center', horizontalalignment='right',
        transform=ax.transAxes,
        color='green', fontsize=16)
    plt.title(title, fontsize='x-large', y = 1.02)
    plt.show()

    print('save plots ... ')
    # save the plot as a file
    fig.savefig(str(title)+'.jpg',
                format='jpeg',
                dpi=150,
                bbox_inches='tight')
    
    # assume that weekly return
    SR = np.sqrt(252)*np.nanmean(np.diff(pnl, prepend=np.nan)/pnl) / np.nanstd(np.diff(pnl, prepend=np.nan)/pnl)
    R2 = 1 - (np.sum((actual-predicted)**2))/np.sum((actual - np.mean(actual))**2)
    
    print('SR_Annualized: ', SR)
    print('R2: ', R2)

#%% DM Test

def dm_test(actual_lst, pred1_lst, pred2_lst, h = 1, crit="MSE", power = 2):
    # Routine for checking errors
    def error_check():
        rt = 0
        msg = ""
        # Check if h is an integer
        if (not isinstance(h, int)):
            rt = -1
            msg = "The type of the number of steps ahead (h) is not an integer."
            return (rt,msg)
        # Check the range of h
        if (h < 1):
            rt = -1
            msg = "The number of steps ahead (h) is not large enough."
            return (rt,msg)
        len_act = len(actual_lst)
        len_p1  = len(pred1_lst)
        len_p2  = len(pred2_lst)
        # Check if lengths of actual values and predicted values are equal
        if (len_act != len_p1 or len_p1 != len_p2 or len_act != len_p2):
            rt = -1
            msg = "Lengths of actual_lst, pred1_lst and pred2_lst do not match."
            return (rt,msg)
        # Check range of h
        if (h >= len_act):
            rt = -1
            msg = "The number of steps ahead is too large."
            return (rt,msg)
        # Check if criterion supported
        if (crit != "MSE" and crit != "MAPE" and crit != "MAD" and crit != "poly"):
            rt = -1
            msg = "The criterion is not supported."
            return (rt,msg)  
        # Check if every value of the input lists are numerical values
        from re import compile as re_compile
        comp = re_compile("^\d+?\.\d+?$")  
        def compiled_regex(s):
            """ Returns True is string is a number. """
            if comp.match(s) is None:
                return s.isdigit()
            return True
        for actual, pred1, pred2 in zip(actual_lst, pred1_lst, pred2_lst):
            is_actual_ok = compiled_regex(str(abs(actual)))
            is_pred1_ok = compiled_regex(str(abs(pred1)))
            is_pred2_ok = compiled_regex(str(abs(pred2)))
            if (not (is_actual_ok and is_pred1_ok and is_pred2_ok)):  
                msg = "An element in the actual_lst, pred1_lst or pred2_lst is not numeric."
                rt = -1
                return (rt,msg)
        return (rt,msg)
    
    # Error check
    error_code = error_check()
    # Raise error if cannot pass error check
    if (error_code[0] == -1):
        raise SyntaxError(error_code[1])
        return
    # Import libraries
    from scipy.stats import t
    import collections
    import pandas as pd
    import numpy as np
    
    # Initialise lists
    e1_lst = []
    e2_lst = []
    d_lst  = []
    
    # convert every value of the lists into real values
    actual_lst = pd.Series(actual_lst).apply(lambda x: float(x)).tolist()
    pred1_lst = pd.Series(pred1_lst).apply(lambda x: float(x)).tolist()
    pred2_lst = pd.Series(pred2_lst).apply(lambda x: float(x)).tolist()
    
    # Length of lists (as real numbers)
    T = float(len(actual_lst))
    
    # construct d according to crit
    if (crit == "MSE"):
        for actual,p1,p2 in zip(actual_lst,pred1_lst,pred2_lst):
            e1_lst.append((actual - p1)**2)
            e2_lst.append((actual - p2)**2)
        for e1, e2 in zip(e1_lst, e2_lst):
            d_lst.append(e1 - e2)
    elif (crit == "MAD"):
        for actual,p1,p2 in zip(actual_lst,pred1_lst,pred2_lst):
            e1_lst.append(abs(actual - p1))
            e2_lst.append(abs(actual - p2))
        for e1, e2 in zip(e1_lst, e2_lst):
            d_lst.append(e1 - e2)
    elif (crit == "MAPE"):
        for actual,p1,p2 in zip(actual_lst,pred1_lst,pred2_lst):
            e1_lst.append(abs((actual - p1)/actual))
            e2_lst.append(abs((actual - p2)/actual))
        for e1, e2 in zip(e1_lst, e2_lst):
            d_lst.append(e1 - e2)
    elif (crit == "poly"):
        for actual,p1,p2 in zip(actual_lst,pred1_lst,pred2_lst):
            e1_lst.append(((actual - p1))**(power))
            e2_lst.append(((actual - p2))**(power))
        for e1, e2 in zip(e1_lst, e2_lst):
            d_lst.append(e1 - e2)    
    
    # Mean of d        
    mean_d = pd.Series(d_lst).mean()
    
    # Find autocovariance and construct DM test statistics
    def autocovariance(Xi, N, k, Xs):
        autoCov = 0
        T = float(N)
        for i in np.arange(0, N-k):
              autoCov += ((Xi[i+k])-Xs)*(Xi[i]-Xs)
        return (1/(T))*autoCov
    gamma = []
    for lag in range(0,h):
        gamma.append(autocovariance(d_lst,len(d_lst),lag,mean_d)) # 0, 1, 2
    V_d = (gamma[0] + 2*sum(gamma[1:]))/T
    DM_stat=V_d**(-0.5)*mean_d
    harvey_adj=((T+1-2*h+h*(h-1)/T)/T)**(0.5)
    DM_stat = harvey_adj*DM_stat
    # Find p-value
    p_value = 2*t.cdf(-abs(DM_stat), df = T - 1)
    
    # Construct named tuple for return
    dm_return = collections.namedtuple('dm_return', 'DM p_value')
    
    rt = dm_return(DM = DM_stat, p_value = p_value)
    
    return rt

#%% Test data

actual = np.array([4948.764, 4946.674, 4947.109, 4946.359, 4946.866, 4948.734,
                       4947.71 , 4947.118, 4945.165, 4944.563, 4944.838, 4944.43,
                       4944.874, 4944.536, 4943.714, 4941.68 , 4939.49 , 4940.621,
                       4940.032, 4939.127, 4938.898, 4939.213, 4939.411, 4939.704,
                       4940.113, 4940.915, 4941.365, 4940.907, 4941.501, 4941.016])

# predicted value is obtained through PCA regression with 2 PCs
predicted = np.array([4949.39754685, 4948.60303899, 4946.71091446, 4947.2178892 ,
                       4946.52232626, 4947.06362092, 4948.59590509, 4947.76838407,
                       4947.01492282, 4945.18612753, 4944.6544401 , 4944.92441486,
                       4944.45275777, 4944.92074686, 4944.47584507, 4943.49872148,
                       4941.58641973, 4939.60930822, 4940.59979161, 4940.11694573,
                       4939.22683406, 4938.91835888, 4939.29573863, 4939.43756573,
                       4939.65912564, 4940.02981157, 4941.06639892, 4941.33113395,
                       4940.97164103, 4941.49919133])
    
predicted2 = np.array([4948.764, 4948.764, 4946.674, 4947.109, 4946.359, 4946.866, 4948.734,
                       4947.71 , 4947.118, 4945.165, 4944.563, 4944.838, 4944.43,
                       4944.874, 4944.536, 4943.714, 4941.68 , 4939.49 , 4940.621,
                       4940.032, 4939.127, 4938.898, 4939.213, 4939.411, 4939.704,
                       4940.113, 4940.915, 4941.365, 4940.907, 4941.501])

#%% Evaluate

#evaluate(actual[1:len(actual)], _naive_forecasting(actual,1))
#evaluate(actual, predicted)
#dm_test(actual[1:len(actual)], predicted[1:len(actual)], _naive_forecasting(actual,1))
#backtesting(actual, predicted)
#mds(actual[1:len(actual)], _naive_forecasting(actual,1))
