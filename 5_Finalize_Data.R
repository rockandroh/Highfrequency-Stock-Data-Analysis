#' --- ----------------------------------------------------------------
#' @name : 5. Finalize the Dataset
#' @author : H.W Roh
#' @source Feature_Collection 
#' --------------------------------------------------------------------
options(scipen = 999)
library(readr)
library(dplyr)
library(plyr)
library(zoo)
library(ggcorrplot)
library(TTR)

#rm(list = ls())
ticker = 'IBM'
k = 60
setwd(paste("C:/Users/ericr/Dropbox/Master_Thesis/Data/", ticker, '/feature/', k,'min',sep=''))
#setwd("/Users/hyunwooroh/Dropbox/Master_Thesis/Data")
getwd()
ticker = 'IBM'
files <- list.files(path = getwd(), pattern = "*.csv", full.names = T)
files

tbl <- ldply(files, read_csv)
dim(tbl)
tbl$mon = as.numeric(tbl$mon)
tbl$mday = as.numeric(tbl$mday)
str(tbl)

# Each NA in the input object is replaced by the most recent non-NA prior to it. 
# If there are no earlier non-NAs then the NA is omitted
df = tbl[,1:ncol(tbl)] %>% do(na.locf(.)) 
df_rate = tbl[,1:ncol(tbl)] %>% do(na.locf(.))

df$return = rep(0, dim(df)[1])
df$return[2:length(df$close)] = log(as.numeric(df$close[2:length(df$close)]) /
                                       as.numeric(df$close[1:(length(df$close)-1)]))
colnames(df)[colnames(df)=="bs ratio"] <- "bs_ratio"
colnames(df)[colnames(df)=="bid_ask_imbalance_total"] = 'bid_ask_imbal'
colnames(df)[colnames(df)=="buy_sell_imbalance_total"] = 'buy_sell_imbal'
colnames(df)
ncol(df)

df = df %>% dplyr::select(min, hour, mon, wday, mday, yday, num_trade, volume, num_tick, open, high, low, close, return,
  eff_spread, q_spread_avg, wait_avg, wait_max, per_midchange, bid_ask_imbal, buy_sell_imbal, depth_imbalance, buy, sell, bs_ratio,ind_avg, ar1_ind, info_shock, trading_cost, info_asym, interaction, 
  TSRV, rvRTS, rQuar, rMPV, mu, sigma, lambda, delta,hk_alpha_sell, hk_beta_sell, hk_alpha_buy, hk_beta_buy,
  )

ncol(df)
cols = colnames(df)
cols
summary(df$return)
sum(is.na(df))
which(is.na(df$depth_imbalance) == T)

sum(is.na(df_rate))

sum(is.na(df))

# Normalize the numeric values to [0,1]

str(df)
df_norm = as.data.frame(apply(df, 2, function(x) (x - min(x))/(max(x)-min(x))))

str(df_rate)
df_rate_norm = as.data.frame(apply(df_rate, 2, function(x) (x - min(x))/(max(x)-min(x))))

head(df_norm)
sum(is.na(df_norm))
summary(df_norm$return)


# Make Technical Features
get_Technical_feature = function(df){
  
  # RSI
  rsi <- RSI(df[,"close"],n = 20)
  
  # Bollinger Bands
  bbands <- BBands( df[,c("high","low","close")], n=20 )
  
  # Aroon
  aroon <- aroon( df[,c("high", "low")], n=20 )
  
  # Parabolic Stop and Reverse
  sar <- SAR(df[,c("high","low")])
  
  # Momentum
  mom <- momentum(df[,"close"], 1)
  
  # Chaikin Accumulation / Distributio (Money flowindex)
  chaikin_ad <- chaikinAD(df[,c("high","low","close")], df[,"volume"])
  
  # Moving Averages
  ema <- EMA(df[,"close"], n=20)
  
  # Directional Movement Index
  # Welles Wilder Directional Movement Index
  adx <- ADX(df[,c("high","low","close")], n=20 )
  
  # MACD
  macd <- MACD( df[,"close"] )
  
  # Stochastics
  stoch <- stoch(df[,c("high","low","close")])
  
  technical = cbind(rsi,bbands,aroon[,'oscillator'],sar,
                    mom,chaikin_ad,ema,adx[,'ADX'],macd[,'signal'],stoch[,'fastK'])
  colnames(technical) =  c('rsi','dn','mavg','up','pctB','oscil','sar','mom', 'chaikin_ad','ema', 'adx','macd','fastK' )
  result = cbind(df,technical)
  return(result)
}

df_final = get_Technical_feature(df)
colnames(df_final)
dim(df_final)
sum(is.na(df_final))

df_final2=na.omit(df_final)
dim(df_final2)
summary(df_final2)
colnames(df_final2)

#install.packages('corrplot')
library(corrplot)
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
fname = paste('C:/Users/ericr/Dropbox/Master_Thesis/Data/data_final_',ticker,'_',k,'_summary_corr_plot.png',sep='')
png(fname, width = 1400, height = 1000)
corrplot(cor(df_final2), col=col2(10), method='square', tl.pos = 'upper')
dev.off()

head(df_final2)

write.csv(df_final2, 
          file = paste('C:/Users/ericr/Dropbox/Master_Thesis/Data/data_final_',ticker,'_',k,'.csv',sep=''), 
          row.names = FALSE)
#plot(seq(1,30000,length.out = length(result2$oscillator)),result2$oscillator, type = 'l')

sum_stat = sub('.*:', '', summary(df_final2))
dim(sum_stat)
sum_stat = rbind(sum_stat, apply(df_final2, 2, sd))

write.csv(sum_stat, 
          file = paste('C:/Users/ericr/Dropbox/Master_Thesis/Data/data_final_',ticker,'_',k,'_summary.csv',sep=''), 
          row.names = FALSE)

