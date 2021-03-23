# Feature Functions

library(highfrequency)
library(data.table)
library(zoo)
library(xts) 
library(dplyr)
library(plyr)
library(tidyverse)
library(quantmod)
library(timeDate)
library(fasttime)
library(lubridate)
library(gsubfn) # list = [a,b]
library(fitdistrplus)
library(ggplot2)
library(ggcorrplot)
library(TTR)
library(hawkes)

printf <- function(...) cat(sprintf(...))

#' --------------------------------------------------------------------
#' Calculate Basic Info
#' --------------------------------------------------------------------
#' 
#' load("D:/STV/S_tq_2018_01_matched_all.RData")

get_tq_basic_info = function(df_matched){
  df = df_matched
  df$IND = getTradeDirection(df)
  df$SPREAD = as.numeric(df$OFR) - as.numeric(df$BID)
  df$MID = (as.numeric(df$OFR) + as.numeric(df$BID))/2
  df$RETURN = rep(0, dim(df)[1])
  df$RETURN[2:length(df$RETURN)] = log(as.numeric(df$PRICE[2:length(df$PRICE)]) /
                                         as.numeric(df$PRICE[1:(length(df$PRICE)-1)]))
  
  df$BUY = df$SIZE
  df$SELL = df$SIZE
  df$SELL[df$IND>0] = 0 
  df$BUY[df$IND<0] = 0
  return(df)
}

get_q_basic_info = function(df_quotes){
  df = df_quotes
  df$MID = (as.numeric(df$OFR) + as.numeric(df$BID))/2
  df$SPREAD = round(as.numeric(df$OFR) - as.numeric(df$BID),3)
  df$RETURN = rep(0, dim(df)[1])
  df$RETURN[2:length(df$RETURN)] = round(log(as.numeric(df$MID[2:length(df$MID)]) /
                                               as.numeric(df$MID[1:(length(df$MID)-1)])),6)
  return(df)
}

get_time = function(df){
  
  output = setNames(data.frame(matrix(ncol = 3, nrow = 1)),
                    c('wday','hour','min'))
  # Time
  output[1,1] = as.POSIXlt(index(df)[1])$wday # 0-6 starting from sunday
  output[1,2] = as.POSIXlt(index(df)[1])$hour # 0-23
  output[1,3] = as.POSIXlt(index(df)[1])$min # 0-59

  printf('time done')
  
  return(output)
}


get_dimension_matched = function(Quotes = q.df, TAQ = tq.df , k = minute){
  
  Quotes_5min = split(Quotes, "minutes", k=k)
  TAQ_5min = split(TAQ, "minutes", k=k)
  
  exclude_quotes = c()
  for (i in 1:length(Quotes_5min)){
    if (dim(Quotes_5min[[i]])[1] < 5) {
      exclude_quotes = c(exclude_quotes, i)
    }
  }
  if (!(length(exclude_quotes) == 0)){
    printf('# of abnormally small tick 5 min period: %d \n',
           (length(Quotes_5min)-length(Quotes_5min[-c(exclude_quotes)])))
    Quotes_5min = Quotes_5min[-(exclude_quotes)]
  }
  
  diff = abs(length(TAQ_5min)-length(Quotes_5min))
  
  exclude=c()
  if(!(diff==0)){
    end = last(sort(sapply(TAQ_5min, nrow))[1:diff])
    
    exclude = c()
    for (i in 1:length(TAQ_5min)){
      if (nrow(TAQ_5min[[i]]) <= end) {
        exclude = c(exclude, i)
      }
    }
    if (!(length(exclude) == 0)){
      #printf('# of abnormally small tick 30min period: %d \n',length(TAQ_5min[-c(exclude)]))
      TAQ_5min = TAQ_5min[-(exclude)]
    }
    
    printf('The difference: %d \n', length(TAQ_5min)-length(Quotes_5min))
  }
  
  if(!(length(TAQ_5min)-length(Quotes_5min)==0))
    stop('error: match not completed')
  
  exclude = list(TAQ_5min,exclude,Quotes_5min,exclude_quotes)
  
  printf('dimension mathced')
  
  return(exclude)
}

get_basic_features_5min = function(df_matched){
  
  df = df_matched
  
  output = setNames(data.frame(matrix(ncol = 15, nrow = 1)),
                    c('wday','hour','min','mon','mday','yday',
                      'open','high','low','close',
                      'num_trade','volume', 'eff_spread', 'buy_sell_imbalance_total','depth_imbalance')
  )
  
  # Time
  output[1,1] = as.POSIXlt(index(df)[1])$wday # 0-6 starting from sunday
  output[1,2] = as.POSIXlt(index(df)[1])$hour # 0-23
  output[1,3] = as.POSIXlt(index(df)[1])$min # 0-59
  output[1,4] = strsplit(strsplit(as.character(index(df)[1]), " ")[[1]][1],"-")[[1]][2] # 1-12 mon
  output[1,5] = strsplit(strsplit(as.character(index(df)[1]), " ")[[1]][1],"-")[[1]][3] # 1-31 day of month
  output[1,6] = as.POSIXlt(index(df)[1])$yday+1 # 1-365
  
  # OHLC
  output[1,7] = to.minutes((df$PRICE),k)[,1]  # open
  output[1,8] = to.minutes((df$PRICE),k)[,2]  # high
  output[1,9] = to.minutes((df$PRICE),k)[,3]  # low
  output[1,10] = to.minutes((df$PRICE),k)[,4] # close
  
  # Others
  output[1,11] = nrow(df$PRICE)          # num_trade
  output[1,12] = sum(df$SIZE)            # volume
  output[1,13] = mean(2*abs(df$PRICE - (df$OFR + df$BID)/2)) # mean effective spread
  output[1,14] = sum(df$SIZE*df$IND) # buy_sell_imblanace
  output[1,15] = mean(as.numeric(df$IND) * as.numeric(((df$OFRSIZ - df$BIDSIZ) / (df$OFRSIZ + df$BIDSIZ)))) # depth_imbalance
  
  return (output)
}

get_spread_feature = function(df_quotes){
  
  df = df_quotes
  df$OFR = as.numeric(df$OFR)
  
  output = setNames(data.frame(matrix(ncol = 3, nrow = 1)),
                    c('num_tick', 'q_spread_avg', 'bid_ask_total')
  )
  
  # Spread
  output[1,1] = nrow(df$OFR)            # num_tick
  output[1,2] = mean(df$SPREAD)         # q_spread_avg
  output[1,3] = sum(df$BIDSIZ - df$OFRSIZ) # bid_ask_imbalance_total


  return (output)
}

get_buysell_feature = function(TAQ_matched){
  
  df = TAQ_matched[,c('BUY','SELL','IND')]
  
  data = data.frame(t(colMeans(df))) # take means
  
  output = setNames(data.frame(matrix(ncol = 5, nrow = 1)), c('buy','sell','bs_ratio', 'ind_avg' ,'ar1_ind'))
  output[1,1] = data[,1]
  output[1,2] = data[,2]
  output[1,3] = data[,1]/data[,2]
  output[1,4] = data[,3]
  output[1,5] = as.numeric(arima(as.numeric(df$IND),order=c(1,0,0))$coef[1]) # AR coef
  
  return (output)
}


source('C:/Users/ericr/Dropbox/Master_Thesis/Features/MRR.R')

get_madhavan_feature = estimate_mrr_modified


get_RV_features = function(Quotes){
  
  df = Quotes
  df$MID = as.character(df$MID)
  
  TSRV_feature = rTSCov(df$MID, K=4, J=1, makePsd = FALSE)
  rvRTS_feature = rRTSCov(df$MID, K=4, J=1, makePsd = FALSE)
  rQuar_feature = rQuar(df$MID, align.by ='seconds', makeReturns=TRUE)
  rMPV_feature = rMPV(df$MID, align.by ='seconds', align.period = 10, makeReturns = TRUE) 
  
  output = setNames(data.frame(matrix(ncol = 4, nrow = 1)), 
                    c('TSRV','rvRTS','rQuar','rMPV'))
  output[1,1] = TSRV_feature
  output[1,2] = rvRTS_feature
  output[1,3] = rQuar_feature
  output[1,4] = rMPV_feature
  
  return(output)
  
}

get_MJD_feature = function(df){
  
  df$PRICE = df$MID
  # calculate the return series
  df$Return = rep(0, length(df$PRICE))
  df$Return[2:length(df$Return)] = 
    log(as.numeric(df$PRICE[2:length(df$PRICE)]) / 
          as.numeric(df$PRICE[1:(length(df$PRICE)-1)]))
  
  # calculation empirical moments
  return.data = df$Return
  n.length = length(return.data)
  emp.moment.1 = mean(return.data)
  emp.moment.2 = 1/(n.length-1)*sum((return.data - emp.moment.1)^2)
  emp.moment.4 = 1/(n.length-1)*sum((return.data - emp.moment.1)^4)
  emp.moment.6 = 1/(n.length-1)*sum((return.data - emp.moment.1)^6)
  
  # optimization function
  moment.diff = function(data.input){
    mu = data.input[1]
    sigma.square = data.input[2]
    lambda = data.input[3]
    delta.square = data.input[4]
    
    theo.moment.1 = mu - sigma.square / 2
    theo.moment.2 = sigma.square + lambda * delta.square
    theo.moment.4 = 3 * ((sigma.square+lambda*delta.square)^2 + lambda*delta.square^2)
    theo.moment.6 = 15 * ((sigma.square+lambda*delta.square)^2 + 3*lambda*delta.square*(sigma.square+lambda*delta.square)+lambda*delta.square^3)
    
    least.square.obj = (theo.moment.1 - emp.moment.1)^2 + 100*(theo.moment.2 - emp.moment.2)^2 + 1000*(theo.moment.4 - emp.moment.4)^2 + 10000*(theo.moment.6 - emp.moment.6)^2
    return(least.square.obj)
  }
  
  # parameter calibration
  output = optim(c(0, 0.0005, 0.0005, 0.001), moment.diff)
  #print(output$par)
  
  mu = output$par[1]
  sigma.square = output$par[2]
  lambda = output$par[3]
  delta.square = output$par[4]
  
  output = setNames(data.frame(matrix(ncol = 4, nrow = 1)), 
                    c('mu','sigma','lambda', 'delta'))
  
  output[1,1]=mu
  output[1,2]=sigma.square
  output[1,3]=lambda
  output[1,4]=delta.square
  
  theo.moment.1 = mu - sigma.square / 2
  theo.moment.2 = sigma.square + lambda * delta.square
  theo.moment.4 = 3 * ((sigma.square+lambda*delta.square)^2 + lambda*delta.square^2)
  theo.moment.6 = 15 * ((sigma.square+lambda*delta.square)^2 + 3*lambda*delta.square*(sigma.square+lambda*delta.square)+lambda*delta.square^3)  
  
  # theo.out = c(theo.moment.1, theo.moment.2, theo.moment.4, theo.moment.6)
  # empi.out = c(emp.moment.1, emp.moment.2, emp.moment.4, emp.moment.6)
  # diff.out = empi.out - theo.out
  # perg.out = diff.out / theo.out
  # 
  # print(cbind(theo.out, empi.out, diff.out, perg.out))
  
  return(output)
}

get_bouncing_feature = function(df){
  
  df$cnt = 0
  count = 0
  for(i in c(2:dim(df)[1])){
    if(as.numeric(df$MID[i])==as.numeric(df$MID[i-1])){
      count = count + 1
    }
    else{
      df$cnt[i]=count
      count=1
    }
  }
  # if (max(df$cnt)==0) lambda =0
  # else {
  #   fit1 = fitdistr(as.numeric(df$cnt[df$cnt>0]), densfun="exponential", lower=c(0,0))
  #   lambda = as.numeric(fit1$estimate)
  # }
  df$PrevMID = lag(df$MID,1)
  df$change = ifelse(df$MID==df$PrevMID,0,1)
  df = df[-1]
  n = dim(df)[1]
  num_midchange = sum(df$change)
  
  output = setNames(data.frame(matrix(ncol = 3, nrow = 1)), 
                    c('wait_avg','wait_max','per_midchange'))
  output[1,1] = mean(as.numeric(df$cnt))
  output[1,2] = max(as.numeric(df$cnt))
  #output[1,3] = lambda
  output[1,3] = num_midchange / n # 99% similarity with lambda
  
  return(output)
}

get_hawkes_feature = function(df){
  
  negloglik_hawkes <- function(params, history) {
    # params == c(mu, alpha, beta)
    return(likelihoodHawkes(params[1], params[2], params[3], history))
  }
  
  fit_hawkes <- function(times) {
    
    ui <- rbind(diag(3), c(0,-1,1))  # constraints: mu,alpha,beta >= 0 && alpha <= beta
    init_par <- c(1, 0.1, 1)         # init params
    constrOptim(init_par, negloglik_hawkes, grad=NULL,
                ui = ui, ci=0, history = times)
  }
  
  #buy
  index = index(df[df$IND==1 & df$SIZE > 5])
  cum_sec_buy = as.numeric(as.POSIXlt(index) - as.POSIXlt(index[1]))
  
  #sell
  index = index(df[df$IND==-1 & df$SIZE > 5])
  cum_sec_sell = as.numeric(as.POSIXlt(index) - as.POSIXlt(index[1]))
  
  output = setNames(data.frame(matrix(ncol = 4, nrow = 1)), 
                    c('hk_alpha_buy','hk_beta_buy','hk_alpha_sell', 'hk_beta_sell'))
  buy_result = fit_hawkes(cum_sec_buy)$par
  sell_result = fit_hawkes(cum_sec_sell)$par
  output[1,1]=buy_result[2]
  output[1,2]=buy_result[3]
  output[1,3]=sell_result[2]
  output[1,4]=sell_result[3]
  
  return(output)
}

get_technical_feature = function(df_matched){
  
  df = df_matched
  
  price = df$PRICE
  cols = c('pct_BBands','momentum','ROC','MACD','MACD_signal','aroon_up','arron_dn','aroon_oscillator','WPR','SMI','signal')
  output = setNames(data.frame(matrix(ncol = length(cols), nrow = 1)), 
                   cols)
  output[1,1] = mean(BBands(price)$pctB, na.rm=TRUE)
  output[1,2] = mean(momentum(price), na.rm=TRUE)
  output[1,3] = mean(ROC(price), na.rm=TRUE)
  output[1,4] = lapply(na.omit(MACD(price)),mean)$macd
  output[1,5] = lapply(na.omit(MACD(price)),mean)$signal
  output[1,6] = lapply(na.omit(aroon(price)),mean)$aroonUp
  output[1,7] = lapply(na.omit(aroon(price)),mean)$aroonDn
  output[1,8] = lapply(na.omit(aroon(price)),mean)$oscillator
  output[1,9] = mean(WPR(price), na.rm=TRUE)
  output[1,10] = lapply(na.omit(SMI(price)),mean)$SMI
  output[1,11] = lapply(na.omit(SMI(price)),mean)$signal
  
  return(output)
  
}

