#A

#1)
library(quantmod)
stocks <- c("MMM","AXP","AAPL","BA","CAT","CVX","CSCO","KO","DIS","DWDP","XOM","GS","HD","IBM",
            "INTC","JNJ","JPM","MCD","MRK","MSFT","NKE","PFE","PG","TRV","UTX","UNH","VZ","V","WMT","WBA")

getSymbols(stocks, src="yahoo", from="1999-12-01", to="2018-12-31")

muF<-function(d,X){mean(X)*sum(d*d)}
varF<-function(d,X){
  M<-length(d)-1
  acfs<- acf(X, plot=F, type="covariance", lag.max=M)$acf
  Gamma<-toeplitz(as.vector(acfs))
  d%*%Gamma%*%as.vector(d)
}
rhoF<-function(d,X){
  M<-length(d)-1
  acfs<- acf(X, plot=F, type ="covariance", lag.max=M+2)$acf
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
  list("ER"=ER, "H"=H, "rhoF"=rF, "VF"=vF, "muF"=mF,
       "corXF"=rXF)
}

#final results for 30 stocks
all_mr <- NULL

for(stock in stocks) {
  
  monthlyData <- monthlyReturn(get(stock),type="log")
  
  #Choose the optimal m and r for monthly data
  result <- numeric(0)
  m <- numeric(0)
  r <- numeric(0)
  for (i in 2:51){
    for (j in (i+1):52){
      if (j >i){
        result <- c(result, ruleReturn(monthlyData, m = j, r = i)[[1]])
        m <- c(m,j)
        r <- c(r,i)
      }
    }
  }
  m_optimal <- m[which.max(result)]
  r_optimal <- r[which.max(result)]
  
  #combine them
  all_mr <- rbind(all_mr, c( m_optimal,  r_optimal))
  
}

row.names(all_mr) <- stocks
colnames(all_mr) <- c("m","r")
#optimal m and r 
all_mr

#2)
returns <- NULL
for(stock in stocks) {
  
  
  monthlyData <- monthlyReturn(get(stock),type="log")
  
  returns  <- cbind(returns, monthlyData)
}  




#EW
Br <- numeric(nrow(returns))
for(i in 1:nrow(returns)) {
  Br[i] <- mean(returns[i,],na.rm = TRUE)
}
#performances  mean and variance of EW weighted portfolio
mean(Br)
var(Br)


#RP
Rp <- numeric(nrow(returns))
sds <- apply(returns,2,sd, na.rm=TRUE)
ws <- (1/sds)/(sum(1/sds))
for(i in 1:nrow(returns)) {
  Rp[i] <-  sum(returns[i,] * ws, na.rm=TRUE)
}
#performances  mean and variance of EW weighted portfolio
mean(Rp)
var(Rp)







#B
#1)

#find delta
delta <- 0.2

sigmat <- NULL

#for all 30 stocks, compute sigma_t
for(i in 1:30) {
  
  temp <- c()
  
  for(t in 14:nrow(returns)) {
    
    temp <- c(temp, 13 * sum((1 - delta)*delta^{0:12}*(returns[(t-1):(t-13),i] -  sum((1 - delta)*delta^{0:12} * returns[(t-1):(t-13),i]))^2))
    
  }
  
  sigmat <- cbind(sigmat, sqrt(temp))
}

sigmat


#2)
#predictive regression

#construct  rs,t/sigma s, t-1 matrix

rst_sigmast <- NULL

for(i in 1:30) {
  
  rst_sigmast <- cbind(rst_sigmast, returns[14:nrow(returns),i]/sigmat[,i])
  
}


#find optimal h for all 30 stocks
optimal_h_30 <- numeric(30)
for(i in 1:30) {
  
  rh <- numeric(12)
  for(h in 1:12) {
    y <- rst_sigmast[(h+1):nrow(rst_sigmast) ,i]
    x <- sign(returns[(h+1):nrow(rst_sigmast),i])
    model <- lm(y ~ x)
    rh[h] <- summary(model)$r.squared
  }
  optimal_h_30[i]<- (1:12)[which.max(rh)]
}

#optimal h for 30 stocks with highest R-squared
names(optimal_h_30) <- stocks
optimal_h_30


#3)
#TSMOM
TSMOM <- numeric(nrow(returns) - 14)
for(i in 14:(nrow(returns))) {
  #weights
  ws <- numeric(30)
  for(j in 1:30) {
    #assume hs = 12 for all stocks
    ws[j] <- sign(returns[(i - 12),j])* 40/100 * sigmat[i-13,j]
  }
  
  TSMOM[i] <-  1/30 * sum(ws * returns[i,], na.rm = TRUE)
}
#performances  mean and variance of TSMOM portfolio
mean(TSMOM)
var(TSMOM)

