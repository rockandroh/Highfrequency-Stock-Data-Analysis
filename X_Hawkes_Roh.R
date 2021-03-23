library(hawkes)

lambda0 = 0.2
alpha = 0.1
beta = 0.5
horizon = 50 # on hour
data = simulateHawkes(lambda0,alpha,beta,horizon)

Nt = c(0,rep(1,length(data[[1]])))

plot(c(0,data[[1]]),cumsum(Nt),
     type='s', xlab = 't', ylab = 'N(t)')
points(data[[1]],rep(0,length(data[[1]])),pch=19,col = 'red')

# plot intensity
conditionalintensity <- function(t) {
    lambda0+ sum(alpha*exp(-beta*(t-data[[1]][data[[1]] < t])))
  }
t <- seq(0, 50, length.out = 1e5)
lambda_values = sapply(t, conditionalintensity)

plot(t, lambda_values, type = 'l', xlab = 't',
     ylab = expression(paste(lambda, '(t)')))
points(data[[1]], rep(1,length(data[[1]]))*lambda0, pch = 19, col = 'red')
legend('topleft', 'jump times', pch = 19, col = 'red')

# plot of true excitement (exponential decay)
mu <- function(t) {
  alpha*exp(-beta*t)
}
plot(t, mu(t), type = 'l', ylab = 'excitement function')

##################################################
## Estimation Hawkes using nonparametric method ##
##                INAR(p)                       ##
##################################################
Hawkes.estimator <- function(delta, s, data) {# s is support, delta is the bin size
  n <- ceiling(T/delta)
  x <- hist(data[[1]], breaks = seq(0, n*delta, by = delta), plot = FALSE)$counts # bin-count seqence
  p <- floor(s/delta) # lag p
  Y <- x[(p+1): n]
  # design matrix
  Z <- x[p:(n-1)]
  for (i in (p-1):1){
    Z <- rbind(Z, x[i:(i+n-p-1)])
  }
  Z <- rbind(Z, 1)
  CLS <- Y %*% t(Z) %*% solve((Z%*%t(Z)))
  H <- CLS/delta # Hawkes estimator
  H
}

## The choice of support
AIC <- function(p){
  H0 <- Hawkes.estimator(delta0, s0, data)
  X <- hist(data[[1]], breaks = seq(0, n0*delta0, by = delta0), plot = FALSE)$counts
  sigma <- 0
  for (k in (p+1):n0) {
    y <- 0
    for (l in 1:p) {
      y <- y + delta*H0[,l] %*% X[k-l]
    }
    u = X[k]-delta*H0[, dim(H0)[2]]-y
    sigma <- sigma + 1/(n0-p)*u %*% t(u)
  }
  d <- dim(H0)[1]
  AIC = determinant(sigma, logarithm = TRUE)$modulus[1]+2*p*d^2/(n0-p)
  AIC
}

d <-1
s0 <- 50
delta0 <- 0.5
n0 <- ceiling(T/delta0)
max.p <- floor(s0/delta0)


p <- c(1:max.p)
aic <- sapply(p, AIC)
plot(p, aic)

p.min <- 1
s <- p.min*delta0


## The choice of bin size
delta <- 0.01
H <- Hawkes.estimator(delta, s, data = data)
baseintensity <- H[, dim(H)[2]] # baseline intensity
h <- function(t){# estimated excitement function
  H[ceiling(t/delta)]
}
delta <- 0.1
t2 <- seq(0, 500, length.out = 1e6)
h(t2)
plot(t2, h(t2))
# smoothing the pointwise estimated values
smooth.h <- ksmooth(t2, h(t2))

plot(smooth.h, type = 'l', xlim=c(0,5))


## diagnostics
# estimated conditional intensity
est.intensity <- function(t) {
  int <- 0
  for(j in 1:d){
    int <- int + sum(h(t-data[[1]])[data[[1]]] < t)
  }
  baseintensity+int
}













set.seed(1)

qqplot(x=qexp(ppoints(100)), y=int.time, main="Exponential Q-Q Plot",
       xlab="Theoretical Quantiles", ylab= "Inter Quantiles")
qqline(data, distribution=qexp)


























#Multivariate Hawkes process
lambda0<-c(0.2,0.2)
alpha<-matrix(c(0.5,0,0,0.5),byrow=TRUE,nrow=2)
beta<-c(0.7,0.7)
horizon<-3600#one hour
h<-simulateHawkes(lambda0,alpha,beta,horizon)