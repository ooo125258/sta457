#A
#1
#install.packages("quantmod")
library(quantmod)
stocks <- c("MMM","AXP","AAPL","BA","CAT","CVX","CSCO","KO","DIS","DWDP","XOM","GS","HD","IBM","INTC",
            "JNJ","JPM","MCD","MRK","MSFT","NKE","PFE","PG","TRV","UTX","UNH","VZ","V","WMT","WBA")

getSymbols(stocks, src="yahoo", from="1999-12-01", to="2018-11-30")

#Loading functions:
muF<-function(d,X){mean(X)*sum(d*d)}
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
  ER<-sqrt(2/pi)*vX*rXF*exp(-mF*mF/(vF*vF))+mX*(1-2*pnorm(-mF/vF))
  H<-Hold(rF)
  list("ER"=ER, "H"=H, "rhoF"=rF, "VF"=vF, "muF"=mF, "corXF"=rXF)
}


#final results for 30 stocks
all_mr <-NULL

for(stock in stocks) {
  
  monthlyData <- monthlyReturn(get(stock), type="log")
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

#2)
#cumulate returns
returns <- NULL
for(stock in stocks){
  monthlyData <- monthlyReturn(get(stock), type="log")
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

