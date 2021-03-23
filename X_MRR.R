# Function for estimating MRR

estimate_mrr_modified <- function(data) 
{
  # make necessary inputs for the GMM
  # price_diff, price, indicator, indicator_lag,midquote, midquote_lag, spread_quoted, volume 
  
  #' @param price_diff a numeric vector containing the series of first price differences.
  n <- nrow(data$PRICE)
  price_diff = as.numeric(data$PRICE[2:n])-as.numeric(data$PRICE[1:n-1])
  #price_diff = as.numeric(SKM_m$PRICE - lag(SKM_m$PRICE,1))[-1]
  
  #' @param price a numeric vector containing the price series.
  price = as.numeric(data$PRICE)[2:n]
  
  #' @param indicator an integer vector containing the trade direction with a buy as 1 and a sell as -1.
  indicator = as.numeric(data$IND)[2:n]
  
  #' @param indicator_lag an integer vector containing the first lag of the indicator series.
  indicator_lag = as.numeric(data$IND)[1:n-1]
  
  #' @param midquote a numeric vector with the midquote price series.
  midquote = ((as.numeric(data$BID)+as.numeric(data$OFR))/2)[2:n]
  
  #' @param midquote_lag a numeric vector containing the first lag of the midquote series.
  midquote_lag = ((as.numeric(data$BID)+as.numeric(data$OFR))/2)[1:n-1]
  
  #' @param spread_quoted a numeric vector containing the quoted spread series. 
  spread_quoted = (as.numeric(data$OFR)-as.numeric(data$BID))[2:n]
  
  #' @param volume a numeric vector containing the size
  volume = as.numeric(data$SIZE)[2:n]
  
  # add here some safeguards 
  n          <- length( price_diff )
  features   <- cbind( price_diff, indicator, indicator_lag, volume ) 
  dmid       <- midquote - midquote_lag
  spread.eff <- 2 * indicator * ( price - midquote ) 
  
  # moments for GMM estimation
  moments <- function( beta, x ) 
  {
    ut   <- (x[,1] - ( beta[2] + beta[1] ) * x[,2] + ( beta[2] + beta[3] * beta[1] ) * x[,3] - beta[4] )
    m1    <- x[,2] * x[,3] - beta[3] * x[,2]^2
    m2    <- ut - beta[4]
    m3    <- (ut - beta[4]) * x[,2]
    m4    <- (ut - beta[4]) * x[,3]
    moms  <- cbind( m1, m2, m3, m4 )
    return( moms )
  }
  
  # extension on volume
  
  moments2 <- function( beta, x ) 
  {
    theta = beta[5]*x[,4] + beta[6]*x[,4]^2
    phi = beta[2]
    rho = beta[3]
    alpha = beta[4]
    ut   <- (x[,1] - ( phi + theta ) * x[,2] + ( phi + rho * theta ) * x[,3] - alpha )
    m1    <- x[,2] * x[,3] - rho * x[,2]^2
    m2    <- ut - alpha
    m3    <- (ut - alpha) * x[,2]
    m4    <- (ut - alpha) * x[,3]
    moms  <- cbind( m1, m2, m3, m4 )
    return( moms )
  }
  
  # estimate GMM model
  result <- gmm::gmm( moments, features, c(theta =0, phi = 0, rho = 0, alpha = 0), 
                      prewhite = 1, kernel = "Bartlett", vcov = "HAC", 
                      bw = sandwich::bwNeweyWest ) 
  # for moments 2
  # result2 <- gmm::gmm( moments2, features, c(theta =0, phi = 0, rho = 0, alpha = 0, beta1 = 0, beta2 = 0), 
  #                     prewhite = 1, kernel = "Bartlett", vcov = "HAC", 
  #                     bw = sandwich::bwNeweyWest ) 
  summry <- summary( result ) 
  
  # create output object
  output <- data.frame( n = integer(), #1
                        theta = double(), theta_std = double(), theta_t = double(), theta_p = double(), #5
                        phi = double(), phi_std = double(), phi_t = double(), phi_p = double(), #9
                        rho = double(), rho_std = double(), rho_t = double(), rho_p = double(), #13
                        alpha = double(), alpha_std = double(), alpha_t = double(), alpha_p = double(), #17
                        r2 = double(), r2_adj = double(), f_test = double(), f_pval = double(), #21
                        theta_start = double(), phi_start = double(), rho_start = double(), alpha_start = double(), #25
                        eps_std = double(),eta_std = double(), spread_eff = double(), spread_eff_std = double(), #29
                        spread_eff_emp = double(), spread_eff_emp_std = double(), #31
                        spread_eff_emp_se = double(), spread_eff_emp_med = double(), #33
                        spread_quoted = double(), spread_quoted_std = double(), #35
                        spread_quoted_se = double(), spread_quoted_med = double(), #37
                        r_mean = double(), r_sd = double(), dprice_vol = double(), #40
                        info_shock = double(), trading_cost = double(), info_asym = double(), interaction = double()
                        ) 
  
  # number of observations
  output[1, 1]  <- result$n  # obs
  
  # theta
  output[1, 2]   <- summry$coefficients[1, 1]  # theta
  output[1, 3]   <- summry$coefficients[1, 2]  # theta se
  output[1, 4]   <- summry$coefficients[1, 3]  # theta t-val
  output[1, 5]   <- summry$coefficients[1, 4]  # theta p-val
  # phi
  output[1, 6]   <- summry$coefficients[2, 1]  # phi
  output[1, 7]   <- summry$coefficients[2, 2]  # phi se
  output[1, 8]   <- summry$coefficients[2, 3]  # phi t-val
  output[1, 9]   <- summry$coefficients[2, 4]  # phi p-val
  # rho
  output[1, 10]  <- summry$coefficients[3, 1]  # rho
  output[1, 11]  <- summry$coefficients[3, 2]  # rho se
  output[1, 12]  <- summry$coefficients[3, 3]  # rho t-val
  output[1, 13]  <- summry$coefficients[3, 4]  # rho p-val
  # alpha
  output[1, 14]  <- summry$coefficients[4, 1]  # alpha
  output[1, 15]  <- summry$coefficients[4, 2]  # alpha se
  output[1, 16]  <- summry$coefficients[4, 3]  # alpha t-val
  output[1, 17]  <- summry$coefficients[4, 4]  # alpha p-val
  
  # statistics
  # (phi+theta)x_t - (phi+roh*theta)x_t-1
  fitted.vals    <- (output[1, 6] + output[1, 2] ) * features[,2] 
  - ( output[1, 6] + output[1, 10] * output[1, 2] ) * features[,3] 
  SSE            <- sum( ( fitted.vals - mean( fitted.vals ) )^2 )
  SST            <- sum( ( features[,1] - mean( features[,1] ) )^2 )
  output[1, 18]  <- SSE / SST # R2
  output[1, 19]  <- 1 - ( result$n - 1 ) / ( result$n - 2 ) * ( 1 - output[1, 18] ) # R adj.
  output[1, 20]  <- ( result$n - 2 ) / 2 * output[1, 18] / ( 1 - output[1, 18] ) # F-stat
  output[1, 21]  <- pf( output[1, 20], result$n - 2, 2, result$n, lower.tail = FALSE ) # F p-val
  
  # starting values
  output[1, 22]  <- 0      # theta start
  output[1, 23]  <- 0      # phi start
  output[1, 24]  <- 0      # rho start
  output[1, 25]  <- 0      # alpha start
  
  # residuals - epsilon & eta 
  epsilon        <- features[,1] - (output[1, 6] + output[1, 2] ) * features[,2] +
                                   ( output[1, 6] + output[1, 10] * output[1, 2] ) * features[,3] 
  output[1, 26]  <- sd(epsilon) # eps_sd
  output[1, 27]  <- sd(price_diff - fitted.vals - epsilon) # eta se
  
  # spreads
  ## estimated effective spread - page 14
  output[1, 28]  <- 2 * ( output[1, 6] + output[1, 2] ) # est. eff. spread // estimated/implied Bid-Ask-Spread
  nabla          <- as.matrix( c( 2, 2, 0, 0 ) )
  vcov           <- as.matrix( result$vcov )
  output[1, 29]  <- sqrt( t( nabla ) %*% vcov %*% nabla ) # est. eff. spread se
  
  ## empirically measured effective spread
  output[1, 30]  <- mean( spread.eff ) # emp. eff. spread mean
  output[1, 31]  <- sd( spread.eff )   # emp. eff. spread sd
  output[1, 32]  <- sd( spread.eff ) / sqrt( n ) # emp. eff. spread se  
  output[1, 33]  <- quantile( spread.eff, probs = c( .5 ), type = 5 ) # emp. eff. spread med
  output[1, 34]  <- mean( spread_quoted ) # quoted spread
  output[1, 35]  <- sd( spread_quoted ) # quoted spread sd
  output[1, 36]  <- sd( spread_quoted ) / sqrt( n ) # quoted spread se
  output[1, 37]  <- quantile( spread_quoted, probs = c( .5 ), type = 5 ) # med. spread quoted
  
  ## Fraction of the implied spread attributable to asymmetric information
  output[1, 38]  <- mean(output[1,2]/(output[1,2]+output[1,6])) # ratio of the information compoenet of the spread mean
  nabla          <- as.matrix( c((output[1,2]+output[1,6]-1)/(output[1,2]+output[1,6])^2,          
                                 -output[1,2]/(output[1,2]+output[1,6])^2, 0, 0 ) )
  vcov           <- as.matrix( result$vcov )
  output[1, 39]  <- sqrt( t( nabla ) %*% vcov %*% nabla ) # ratio of the information compoenet of the spread se
  
  ## The Determinants of Price Volatility
  output[1,40]   <- output[1,26]^2 +
    (output[1,2]+output[1,6])^2 + (output[1,2]*output[1, 10]+output[1,6])^2 
  - 2*(output[1,2]+output[1,6])*(output[1,2]*output[1, 10]+output[1,6])
  
  output[1,41] = (output[1,26]^2) / output[1,40] # public info shock portion 
  output[1,42] = (2*(1-output[1,10])*output[1,6]^2) / output[1,40] # trading cost portion
  output[1,43] = ((1-output[1,10]^2)*output[1,2]^2) / output[1,40] # info_asymmetry portion
  output[1,44] = (2*(1-output[1,10]^2)*output[1,2]*output[1,6]) / output[1,40] # interaction
  return(output[1,c(41,42,43,44)])
  
}
