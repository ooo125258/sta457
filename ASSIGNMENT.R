#A
#1
#install.packages("quantmod")
library(quantmod)
stocks <- c("MMM","AXP","AAPL","BA","CAT","CVX","CSCO","KO","DIS","DWDP","XOM","GS","HD","IBM","INTC",
            "JNJ","JPM","MCD","MRK","MSFT","NKE","PFE","PG","TRV","UTX","UNH","VZ","V","WMT","WBA")
dj30 = new.env()
getSymbols(stocks, env=dj30, src="yahoo", from="1999-12-01", to="2018-12-31", adjust=TRUE)
#Loading functions:
muF<-function(d,X){mean(X)*sum(d)}
# Calculate the variance of forecaster using quadratic form
# d: vector of dj coefficients (j=0, ..., m-2)
# X: log returns
varF<-function(d,X){
  M<-length(d)-1
  acfs<- acf(X, plot=F, type="covariance", lag.max=M)$acf
  Gamma<-toeplitz(as.vector(acfs))
  d%*%Gamma%*%as.vector(d)
}
# Calculate ACF(1) of forecaster using matrix operation and outer function in r
rhoF<-function(d,X){
  M<-length(d)-1
  acfs<- acf(X, plot = F, type = "covariance", lag.max=M+2)$acf#M+2
  temp<-d%*%matrix(acfs[abs(outer(0:M,1:(M+1), "-")) +1,,1],
                   M+1, M+1) %*% as.vector(d)
  temp/varF(d,X)
}
corXF<-function(d,X){
  Mp<-length(d)
  acfs<- acf(X, plot=F, type= "covariance", lag.max=Mp)$acf
  sum(d*acfs[-1])/sqrt(acfs[1]*varF(d,X))
}
Hold<-function(rho){pi/acos(rho)}
# m > r >=1
d<-function(m,r){ c((m-r)*((0:(r-1))+1), r*(m-(r: (m-1))-1))}
# retX: log asset return
# m: long-term MA
# r: short-term MA
ruleReturn<-function(retX, m, r){
  vX<-sd(retX)
  mX<-mean(retX)
  mF<-muF(d(m,r),retX)
  vF<-sqrt(varF(d(m,r),retX))
  rXF<-corXF(d(m,r),retX)
  rF<-rhoF(d(m,r),retX)
  ER<-sqrt(2/pi)*vX*rXF*exp(-mF*mF/(2*vF*vF))+mX*(1-2*pnorm(-mF/vF))
  H<-Hold(rF)
  list("ER"=ER, "H"=H, "rhoF"=rF, "VF"=vF, "muF"=mF, "corXF"=rXF)
}


#final results for 30 stocks
all_mr <-NULL

for(stock in stocks) {
  stockAdjusted = dj30[[stock]][,paste(stock, ".Adjusted",sep="")]
  monthlyData = coredata(diff(log(apply.monthly(stockAdjusted, last))))
  monthlyData=na.omit(monthlyData)
  #monthlyData <- monthlyReturn(get(stock), type="log")
  result<- numeric(0)
  m <- numeric(0)
  r <- numeric(0)
  for (i in 2:11){
    for(j in (i+1):12){
      if (j>i){
        result <- c(result, ruleReturn(monthlyData, m = j, r = i)[[1]])
        m <- c(m,j)
        r <- c(r,i)
      }
    }
  }
  m_optimal <- m[which.max(result)]
  r_optimal <- r[which.max(result)]
  #list(optimal_m = m_optimal, optimal_r = r_optimal)
  
  all_mr <- rbind(all_mr, c(m_optimal, r_optimal))
  
}

row.names(all_mr) <- stocks
colnames(all_mr) <- c("m", "r")
all_mr



ew_q2 = c()
rp_q2 = c()
counters = c()
for(stock in stocks) {
  counters = c(counters, stock)
  stockAdjusted = dj30[[stock]][,paste(stock, ".Adjusted",sep="")]
  monthlyData = coredata(diff(log(apply.monthly(stockAdjusted, last))))
  monthlyData=na.omit(monthlyData)
  monthlyReturn()
  #dj30 = new.env()
  #getSymbols(stocks, env=dj30, src="yahoo", from="1999-12-01", to="2018-11-30", adjust=TRUE)
  mData = coredata(monthlyData)
  in_sample_estimate <- function(X,m, r){#X->data
    #X <- diff(log(data))
    d <- function(j){
      if (j >= 0 & j <= r-1) {(m-r)*(j+1)}
      else if (j >= r & j <= (m-1)) {r*(m-j-1)}
    }
    f <- function(t){
      if (t >= m){
        output <- 0
        for (j in 0:(m-1)){
          output <- output + d(j)*X[t-j]
        }
        output
      }
      else {print('t is smaller than m')}
    }
    #the realized return for daily data
    re <- numeric(0)
    for (t in (m+1):length(X)){
      re <- c(re, sign(f(t-1))*X[t])
    }
    return <- sum(re)/length(m:length(X))
    
  }
  #Return for weekly data
  ew_q2=c(mean_q2,in_sample_estimate(mData,12,11)[[1]]) #This is a vector. 
   rp_q2=c(var_q2,ruleReturn(monthlyData,12,11)[[1]])
}

perf_ew = ((12 * mean(ew_q2) - 0.02) / (sqrt(12) * sqrt(var(ew_q2))))
perf_rp = ((12 * mean(rp_q2) - 0.02) / (sqrt(12) * sqrt(var(rp_q2))))

performance









#2)
#cumulate returns
returns <- NULL
for(stock in stocks){
  stockAdjusted = dj30[[stock]][,paste(stock, ".Adjusted",sep="")]
  monthlyData = coredata(diff(log(apply.monthly(stockAdjusted, last))))
  monthlyData=na.omit(monthlyData)
  #monthlyData <- monthlyReturn(get(stock), type="log")
  returns <- cbind(returns, monthlyData)
}
#R^(EW)_1 = 1/30 \Sigma_i B_(i,t-1) *  r_(i,t)
EW <- numeric(nrow(returns))
for (i in 1:nrow(returns)){
  EW[i] <- mean(returns[i,], na.rm=TRUE)
}
mean(EW)
var(EW)

#R_t^(RP) = \Sigma _i ^30 w_(i,t-1) * B_(i,t-1) *  r_(i,t)
RP <- numeric(nrow(returns))
std_devs=apply(returns, 2, sd, na.rm=TRUE)
weights <- (1/std_devs)/(sum(1/std_devs))
for(i in 1:nrow(returns)){
  RP[i] <- sum(returns[i,] * weights, na.rm=TRUE)
}
mean(RP)
var(RP)

perf_ew1 = ((12 * mean(EW) - 0.02) / (sqrt(12) * sqrt(var(EW))))
perf_rp1 = ((12 * mean(RP) - 0.02) / (sqrt(12) * sqrt(var(RP))))


#PART B QUESTION 1: 
  
  #find delta 
  f <- function(s) {
    sum((1 - s)* s^{0:12}) - 1
  }

res <- optim(0.0001, f, lower = 0)#???????
delta <- res$par
delta

stocks <- c("MMM","AXP","AAPL","BA","CAT","CVX","CSCO","KO","DIS","DWDP","XOM","GS","HD","IBM","INTC",
            "JNJ","JPM","MCD","MRK","MSFT","NKE","PFE","PG","TRV","UTX","UNH","VZ","V","WMT","WBA")
dj30_last5 = new.env()

getSymbols(stocks, env=dj30_last5, src="yahoo", from="2014-01-01", to="2018-12-31", adjust=TRUE)#last 5 years
returns_last5=c()
sigmat <- c()
for(stock in stocks){
  #stockAdjusted = dj30_last5[[stock]][,paste(stock, ".Adjusted",sep="")]
  #monthlyData = coredata(diff(log(apply.monthly(stockAdjusted, last))))
  #monthlyData=na.omit(monthlyData)
  monthlyData <- monthlyReturn(get(stock), type="log")
  returns_last5 <- cbind(returns_last5, monthlyData)
}
delta<-0.2
#for all 30 stocks, compute its sigmat
for(i in 1:30) {
  
  temp <- c()
  
  for(t in 13:nrow(returns_last5)) {#13 or 14?
    #change to 12
    temp <- c(temp, 12 * sum((1 - delta)*delta^{0:11}*(returns_last5[(t-1):(t-12),i] - sum(1 - delta^{0:11} * returns_last5[(t-1):(t-12),i]))^2))
    
  }
  
  sigmat <- cbind(sigmat, sqrt(temp))
}

sigmat

#b-QUESTION 2 
#Predictive regression 
#construct rs, t/sigma s, t-1 matrix

rst_sigmast <- NULL

for(i in 1:30) {
  
  rst_sigmast <- cbind(rst_sigmast, returns_last5[13:nrow(returns_last5),i]/sigmat[,i])
  
  
}

#find optimal h for all 30 stocks
optimal_h_30 <- numeric(30)
for(i in 1:30) {
  
  rh <- numeric(12)
  for(h in 1:12) {
    y <- rst_sigmast[(h+1):nrow(rst_sigmast),i]
    x <- sign(returns_last5[(h+1):nrow(rst_sigmast),i])
    model <- lm(y ~ x)
    rh[h] <- summary(model)$r.squared
  }
  optimal_h_30[i]<- (1:12)[which.max(rh)]
}

#optimal h for 30 stocks with highest R-square value
names(optimal_h_30) <- stocks
optimal_h_30

#Question 3
#TSMOM
TSMOM <- numeric(nrow(returns_last5) - 12)
for(i in 13:(nrow(returns_last5))) {
  #weights
  ws <- numeric(30)
  for(j in 1:30) {
    #assume hs = 12 for all stocks
    weights[j] <- sign(returns_last5[(i -12),j])* 40/100 * sigmat[i-12,j]
  }
  
  TSMOM[i] <- 1/30 * sum(weights * returns_last5[i,], na.rm = TRUE)
}
#performances mean and vavriance of TSMOM portfolio
mean(TSMOM)
var(TSMOM)
perf_tsmom = ((12 * mean(TSMOM) - 0.02) / (sqrt(12) * sqrt(var(TSMOM))))
perf_tsmom
#PARTC
#Question changed:
#The same as the 1st question.
#The formula of ERT is in the discussion

