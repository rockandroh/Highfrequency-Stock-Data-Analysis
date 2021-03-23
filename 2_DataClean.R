#' --- ----------------------------------------------------------------
#' 
#' @name : 2. Data Load
#' @author : H.W Roh
#' 
#' --------------------------------------------------------------------

rm(list = ls())
gc()

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
source("C:/Users/ericr/Dropbox/Master_Thesis/2_DataClean/hf_datacleaning_sources.R")
source("C:/Users/ericr/Dropbox/Master_Thesis/2_DataClean/hf_dataload_clean.R")
source("C:/Users/ericr/Dropbox/Master_Thesis/1_DataReady/realized_source.R")

Sys.setenv(TZ = "GMT")
options(digits.secs = 6)
getOption("digits.secs")
options(scipen = 999)


#' --- ----------------------------------------------------------------
#' 
#' Load the xts data to clean and match the quotes and trades
#' 
#' --------------------------------------------------------------------

#2020

ticker='IBM'
ticker='DELL'
year = '2020'
# Jan
gc()
load_monthly_and_clean(Stock=ticker,year=year,mon='01', first='01',last='31')
Sys.sleep(10)

# Feb
gc()
load_monthly_and_clean(Stock=ticker,year=year,mon='02', first='01',last='29')
Sys.sleep(10)

# Mar
gc()
load_monthly_and_clean(Stock=ticker,year=year,mon='03', first='01',last='31')
Sys.sleep(10)

# Apr
gc()
load_monthly_and_clean(Stock=ticker,year=year,mon='04', first='01',last='30')
Sys.sleep(10)

# May
gc()
load_monthly_and_clean(Stock=ticker,year=year,mon='05', first='01',last='31')
Sys.sleep(10)

# Jun
gc()
load_monthly_and_clean(Stock=ticker,year=year,mon='06', first='01',last='30')
Sys.sleep(10)
