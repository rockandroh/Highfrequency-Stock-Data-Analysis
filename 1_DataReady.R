#' --- ----------------------------------------------------------------
#' 
#' @name : 1. Data Ready 
#' @author : H.W. sRoh
#' @description: We have a set of files downloaded from WRDS database
#' Trades and Quotes folder include a data file of trades and quotes
#' --------------------------------------------------------------------

#### Set Up ####
library(zoo)
library(xts)
library(highfrequency)
library(dplyr)
library(plyr)
library(devtools)
library(tidyverse)
library(TTR)
library(quantmod)
library(progress)
library(timeDate)
library(fasttime)
library(MASS)
library(data.table)
library(lubridate)
source("C:/Users/ericr/Dropbox/Master_Thesis/1_DataReady/realized_source.R")

# Data load from WRDS


# --- Step 1 ---------------------------------------------------------
# Load / transform trade data. 
# --------------------------------------------------------------------
rm(list = ls())
#dev.off()
Sys.setenv(TZ = "GMT")
options(digits.secs = 6)
getOption("digits.secs")
options(scipen = 999)
printf <- function(...) cat(sprintf(...))
gc()
ticker = 'DELL'
col.trades = c("SYMBOL","DATE","TIME","PRICE","SIZE","G127","CORR","COND","EX")
col.quotes = c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")

# Split by ticker
split_agg_WRDS_data <- function(data, output_location, date, TAQ=trades)
{  
  curr_dir = getwd()
  new_dir = setwd(output_location)
  col.trades = c("SYMBOL","DATE","TIME","PRICE","SIZE","G127","CORR","COND","EX")
  col.quotes = c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
  
  if (TAQ=='trades')
    
    for(s in levels(data$SYMBOL))
    {
      subset = data[data$SYMBOL == s,]
      
      for(col in col.trades)
      {
        idx = which(colnames(subset) %in% col)
        if(length(idx) == 0)
          subset[,col] = rep(0,dim(subset)[1])
      }
      subset = subset[,col.trades]
      fname = paste(s,"_",date,"_trades.csv",sep="")
      write.csv(subset,file=fname,row.names=FALSE)
    }
  
  if (TAQ=='quotes')
    
    for(s in levels(data$SYMBOL))
    {
      subset = data[data$SYMBOL == s,]
      
      for(col in col.quotes)
      {
        idx = which(colnames(subset) %in% col)
        if(length(idx) == 0)
          subset[,col] = rep(0,dim(subset)[1])
      }
      subset = subset[,col.quotes]
      fname = paste(s,"_",date,"_quotes.csv",sep="")
      write.csv(subset,file=fname,row.names=FALSE)
    }
  setwd(curr_dir)
}

# Trades
ticker = 'DELL'
setwd(paste('D:/',ticker,'_Trades',sep=''))
wd = getwd()
list = list.files()
list
for (file in list){
  fname = list.files(paste('D:/',ticker,'_Trades/',file,sep=''),full.names = TRUE)
  trades = read.csv(fname,header = T)
  trades = trades %>% dplyr::select('SYM_ROOT','DATE','TIME_M','PRICE','SIZE','TR_SEQNUM','TR_CORR','TR_SCOND','EX')
  colnames(trades) = c("SYMBOL","DATE","TIME","PRICE","SIZE","G127","CORR","COND","EX")
  split_agg_WRDS_data(trades,output_location=paste("D:/WRDS_Trades/",ticker,sep=''), date = file, TAQ='trades')
  printf('%s: Done #Obs: %d \n', file, dim(trades)[1])
  gc()
}

# Quotes
setwd(paste('D:/',ticker,'_Quotes',sep=''))
wd = getwd()
list = list.files()
list

for (file in list){
  fname = list.files(paste('D:/',ticker,'_Quotes/',file,sep=''),full.names = TRUE)
  quotes = read.csv(fname,header = T)
  quotes = quotes %>% dplyr::select('DATE','TIME_M','EX','SYM_ROOT','SYM_SUFFIX','BID','BIDSIZ','ASK','ASKSIZ','QU_COND')
  colnames(quotes) = c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
  split_agg_WRDS_data(quotes,output_location=paste("D:/WRDS_Quotes/",ticker,sep=''), date = file, TAQ='quotes')
  printf('%s: Done #Obs: %d \n', file, dim(quotes)[1])
  gc()
}

# Ready for WRDS to R xts data
# --- Step 2 ---------------------------------------------------------
# Transform trade/quotes data. 
# --------------------------------------------------------------------
transform_xts = function(from= startdate, to= enddate, ticker, datasource_t, datasource_q, trades = TRUE){
  

  datadestination_t = paste('D:/WRDS_Trades/', ticker, '/xts_data', sep='')
  datadestination_q = paste('D:/WRDS_Quotes/', ticker, '/xts_data', sep='')
  
  # "%Y%m%d %H:%M:%OS": Upto milisecond
  
  col.trades = c("SYMBOL","DATE","TIME","PRICE","SIZE","G127","CORR","COND","EX")
  col.quotes = c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
  
  if(trades){
    # Trades
    convert(from=from, to=to, datasource=datasource_t, 
            datadestination=datadestination_t, trades = T,  quotes = F, 
            ticker=ticker, dir = TRUE, extension = "csv",  header = TRUE, 
            tradecolnames = col.trades , quotecolnames = NULL, 
            format="%Y%m%d %H:%M:%OS", onefile = TRUE )
  }
  
  else {
    # Quotes
    convert(from=from, to=to, datasource=datasource_q, 
            datadestination=datadestination_q, trades = F,  quotes = T, 
            ticker=ticker, dir = TRUE, extension = "csv",  header = TRUE, 
            tradecolnames = col.quotes , quotecolnames = NULL, 
            format="%Y%m%d %H:%M:%OS", onefile = TRUE )
  }
  gc()
}


# Trades
ticker = 'DELL'
setwd(paste("D:/WRDS_Trades/",ticker, sep=''))
myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles
read_trade = function(list_of_fnames){
  return = ldply(list_of_fnames, read_csv, 
                 col_types = cols(SYMBOL = col_character(),
                                  DATE = col_double(),
                                  TIME = col_time(format = "%H:%M:%OS"),
                                  PRICE = col_double(),
                                  SIZE = col_double(),
                                  G127 = col_double(),
                                  CORR = col_double(),
                                  COND = col_character(),
                                  EX = col_character()))
  return(return)
}

trades = read_trade(myfiles)
trades = trades %>% arrange(DATE)
is.unsorted(trades$DATE)
sum(is.na(trades))
trades_omit = na.omit(trades) %>% arrange(DATE)
sum(is.na(trades_omit))
fpath_trades = paste('D:/WRDS_Trades/',ticker,'/Agg/',ticker,'_trades.csv', sep='')
write.csv(x = trades, fpath_trades, row.names = FALSE)
rm(trades)
gc()

startdate = "2020-01-01" 
enddate = "2020-06-31"
datasource_t = paste("D:/WRDS_Trades/", ticker, '/Agg',sep='')
datasource_q = paste("D:/WRDS_Quotes/", ticker, '/Agg',sep='')
transform_xts(startdate, enddate, ticker, datasource_t, datasource_q, trades = TRUE)
gc()


#Quotes
read_quote = function(list_of_fnames){
  return = ldply(list_of_fnames, read_csv, 
                 col_types = cols(
                   DATE = col_double(),
                   TIME = col_time(format = "%H:%M:%OS"),
                   EX = col_character(),
                   SYMBOL = col_character(),
                   SYM_SUFFIX = col_logical(),
                   BID = col_double(),
                   BIDSIZ = col_double(),
                   OFR = col_double(),
                   OFRSIZ = col_double(),
                   MODE = col_character()
                 ))
  return(return)
}
setwd("D:/WRDS_Quotes")
myfiles = list.files(path=paste("D:/WRDS_Quotes/",ticker, sep=''), pattern="*.csv", full.names=TRUE)
substring(myfiles[1], 26,28)
for (i in seq(1,length(myfiles),2)){
  quotes =  read_quote(myfiles[i:(i+1)])
  quotes = quotes %>% arrange(DATE)
  is.unsorted(quotes$DATE)
  month = substring(myfiles[i],26,28)
  fpath_quotes = paste('D:/WRDS_Quotes/',ticker,'/Agg/',month,'/',ticker,'_quotes.csv', sep='')
  write.csv(x = quotes, fpath_quotes , row.names = FALSE)
  printf('%s: Done #Obs: %d \n', month, dim(quotes)[1])
  rm(quotes)
  gc()
  Sys.sleep(10)
}


# Transform
ticker = 'DELL'

# Jan
startdate = "2020-01-01" 
enddate = "2020-01-31"
datasource_q = paste("D:/WRDS_Quotes/", ticker, '/Agg/Jan',sep='')
transform_xts(startdate, enddate, ticker,datasoucre_t, datasource_q, trades = FALSE)
gc()
Sys.sleep(10)

# Feb
startdate = "2020-02-01" 
enddate = "2020-02-29"
datasource_q = paste("D:/WRDS_Quotes/", ticker, '/Agg/Feb',sep='')
transform_xts(startdate, enddate, ticker,datasoucre_t, datasource_q, trades = FALSE)
gc()
Sys.sleep(10)

# Mar
startdate = "2020-03-01" 
enddate = "2020-03-31"
datasource_q = paste("D:/WRDS_Quotes/", ticker, '/Agg/Mar',sep='')
transform_xts(startdate, enddate, ticker,datasoucre_t, datasource_q, trades = FALSE)
gc()
Sys.sleep(10)

# Apr
startdate = "2020-04-01" 
enddate = "2020-04-30"
datasource_q = paste("D:/WRDS_Quotes/", ticker, '/Agg/Apr',sep='')
transform_xts(startdate, enddate, ticker,datasoucre_t, datasource_q, trades = FALSE)
gc()
Sys.sleep(10)

# May
startdate = "2020-05-01" 
enddate = "2020-05-31"
datasource_q = paste("D:/WRDS_Quotes/", ticker, '/Agg/May',sep='')
transform_xts(startdate, enddate, ticker,datasoucre_t, datasource_q, trades = FALSE)
gc()
Sys.sleep(10)

# June
startdate = "2020-06-01" 
enddate = "2020-06-30"
datasource_q = paste("D:/WRDS_Quotes/", ticker, '/Agg/Jun',sep='')
transform_xts(startdate, enddate, ticker,datasoucre_t, datasource_q, trades = FALSE)
gc()
Sys.sleep(10)


















































