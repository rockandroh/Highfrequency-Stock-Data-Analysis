#' --- ----------------------------------------------------------------
#' 
#' @name : 4. Prepare Data for Analysis
#' @author : H.W Roh
#' @source Feature_Collection 
#' --------------------------------------------------------------------

source('C:/Users/ericr/Dropbox/Master_Thesis/3_FeatureGenerating/3_Feature_Collection.R')

gc()
#rm(list = ls())
ticker = 'DELL'

Stock = ticker; year='2020'; mon='01'; first='01'; last='31'; k=60

Prepare_Data = function(Stock = ticker, year='2020', mon='01',first='01',last='31', k=5){
  setwd("D:/WRDS_Matched")
  fname_tq = paste(Stock,"tq",year,mon,"matched",sep = "_") #"IBM_tq_2020_01_matched"
  fpath_tq = paste("D:/WRDS_Matched/", Stock, '/', fname_tq,".RData",sep="") #"D:/WRDS_Matched/IBM/IBM_tq_2020_01_matched.RData"
  
  load(fpath_tq)
  
  tq.df = get_tq_basic_info(tq.df)
  q.df= get_q_basic_info(q.df)
  
  list[TAQ_5min,exclude,Quotes_5min,exclude_quotes] = get_dimension_matched(q.df, tq.df, k)
  
  basic_feature = as.data.frame(do.call(rbind, lapply(TAQ_5min, get_basic_features_5min)))
  printf('Basic Feature Done %s - %s \n', year, mon)

  spread_feature = as.data.frame(do.call(rbind, lapply(Quotes_5min, get_spread_feature)))
  printf('Spread Feature Done %s - %s \n', year, mon)
  
  bs_feature = as.data.frame(do.call(rbind, lapply(TAQ_5min, get_buysell_feature)))
  printf('Buy/sell Feature Done %s - %s \n', year, mon)
  
  madhavan_feature = as.data.frame(do.call(rbind, lapply(TAQ_5min, get_madhavan_feature)))
  printf('Madhavan Feature Done %s - %s \n', year, mon)
  
  rv_feature = as.data.frame(do.call(rbind, lapply(Quotes_5min, get_RV_features)))
  printf('RV Feature Done %s - %s \n', year, mon)
  
  MJD_feature = as.data.frame(do.call(rbind, lapply(Quotes_5min, get_MJD_feature)))
  printf('MJD Feature Done %s - %s \n', year, mon)
  
  bouncing_feature = as.data.frame(do.call(rbind, lapply(TAQ_5min, get_bouncing_feature)))
  printf('Bouncing Feature Done %s - %s \n', year, mon)
  
  hawkes_feature = as.data.frame(do.call(rbind, lapply(TAQ_5min, get_hawkes_feature)))
  printf('Hawkes Feature Done %s - %s \n', year, mon)
  
  #technical_feature = as.data.frame(do.call(rbind, lapply(TAQ_5min, get_technical_feature)))
  #printf('Technical Feature Done %s - %s \n', year, mon)
  
  data_ready = cbind(basic_feature, spread_feature, bs_feature,
                     madhavan_feature, rv_feature, MJD_feature,
                     bouncing_feature, hawkes_feature)

  fname_tq_2 = paste(Stock,"tq",year,mon,"feature",k,'minute',sep = "_") #"IBM_tq_2020_01_feature"
  fpath_tq_2 = paste("D:/WRDS_Matched/",Stock,"/feature/",fname_tq_2,".csv",sep="") #"D:/STV/t.dfq_2019_01_feature.csv"
  
  write.csv(data_ready, file = fpath_tq_2, row.names = FALSE)
  
  printf('Feature Generation Complete for %s - %s \n', year, mon)
  
  gc()
}

year = 2020
ticker = 'IBM'

Prepare_Data(Stock=ticker,year=year,mon='01', first='01',last='31', k=3)
Prepare_Data(Stock=ticker,year=year,mon='02', first='01',last='29', k=3)
Prepare_Data(Stock=ticker,year=year,mon='03', first='01',last='31', k=3)
Prepare_Data(Stock=ticker,year=year,mon='04', first='01',last='30', k=3)
Prepare_Data(Stock=ticker,year=year,mon='05', first='01',last='31', k=3)
Prepare_Data(Stock=ticker,year=year,mon='06', first='01',last='30', k=3)

Prepare_Data(Stock=ticker,year=year,mon='01', first='01',last='31', k=15)
Prepare_Data(Stock=ticker,year=year,mon='02', first='01',last='29', k=15)
Prepare_Data(Stock=ticker,year=year,mon='03', first='01',last='31', k=15)
Prepare_Data(Stock=ticker,year=year,mon='04', first='01',last='30', k=15)
Prepare_Data(Stock=ticker,year=year,mon='05', first='01',last='31', k=15)
Prepare_Data(Stock=ticker,year=year,mon='06', first='01',last='30', k=15)

Prepare_Data(Stock=ticker,year=year,mon='01', first='01',last='31', k=60)
Prepare_Data(Stock=ticker,year=year,mon='02', first='01',last='29', k=60)
Prepare_Data(Stock=ticker,year=year,mon='03', first='01',last='31', k=60)
Prepare_Data(Stock=ticker,year=year,mon='04', first='01',last='30', k=60)
Prepare_Data(Stock=ticker,year=year,mon='05', first='01',last='31', k=60)
Prepare_Data(Stock=ticker,year=year,mon='06', first='01',last='30', k=60)



