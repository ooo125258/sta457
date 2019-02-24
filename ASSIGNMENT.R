#A
#1
#install.packages("quantmod")
library(quantmod)
stocks <- c("MMM","AXP","AAPL","BA","CAT","CVX","CSCO","KO","DIS","DWDP","XOM","GS","HD","IBM","INTC",
            "JNJ","JPM","MCD","MRK","MSFT","NKE","PFE","PG","TRV","UTX","UNH","VZ","V","WMT","WBA")
dj30 = new.env()
getSymbols(stocks,src="yahoo", from="1999-12-31", to="2018-12-31")
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


all_mr_double_ma <-NULL

#Q6 from 2018 assignment
for(stock in stocks) {
  #stockAdjusted = dj30[[stock]][,paste(stock, ".Adjusted",sep="")]
  #monthlyData = coredata(diff(log(apply.monthly(stockAdjusted, last))))
  #monthlyData=na.omit(monthlyData)
  #https://www.rdocumentation.org/packages/quantmod/versions/0.4-13/topics/periodReturn
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
  
  #collect all m and r for double ma rule
  all_mr_double_ma <- rbind(all_mr_double_ma, c(m_optimal, r_optimal))
  
}

#add title for the result
row.names(all_mr_double_ma) <- stocks
colnames(all_mr_double_ma) <- c("m", "r")
all_mr_double_ma


#2)
#cumulate returns
returns <- NULL
#In this question, I collect data from 5 years, 60 month period
getSymbols(stocks, src="yahoo", from="2014-01-01", to="2018-12-31")


for(stock in stocks){
  #stockAdjusted = dj30[[stock]][,paste(stock, ".Adjusted",sep="")]
  #monthlyData = coredata(diff(log(apply.monthly(stockAdjusted, last))))
  #monthlyData=na.omit(monthlyData)
  #na_fill = rep(NA,228-length(monthlyData))
  #monthlyData=c(na_fill, monthlyData)
  #monthlyData <- monthlyReturn(dj30[[stock]][,paste(stock, ".Adjusted",sep="")], type="log")
  monthlyData<- monthlyReturn(get(stock), type="log")
  returns <- cbind(returns, monthlyData)
}
#R^(EW)_1 = 1/30 \Sigma_i B_(i,t-1) *  r_(i,t)
#The formula of R_t^EW expression, the average of sum rulereturn
EW <- numeric(nrow(returns))
for (i in 1:nrow(returns)){
  #remove the na, for any exception.
  EW[i] <- mean(returns[i,], na.rm=TRUE)
}
#show the result of EW
mean(EW)
var(EW)

#R_t^(RP) = \Sigma _i ^30 w_(i,t-1) * B_(i,t-1) *  r_(i,t)
#The formula of R_t^RP expression, the average of weighted ruleReturn
RP <- numeric(nrow(returns))
#standard derivation of returns
std_devs=apply(returns, 2, sd, na.rm=TRUE)
#The thing before B
weights <- (1/std_devs)/(sum(1/std_devs))
for(i in 1:nrow(returns)){
  RP[i] <- sum(returns[i,] * weights, na.rm=TRUE)
}
#Show the result of RP
mean(RP)
var(RP)

#Show performance: Sharpe ratio for Each of ew and rp
perf_ew1 = ((12 * mean(EW) - 0.02) / (sqrt(12) * sqrt(var(EW))))
perf_rp1 = ((12 * mean(RP) - 0.02) / (sqrt(12) * sqrt(var(RP))))
perf_ew1
perf_rp1


#PART B QUESTION 1: 
  #find delta with the formula provided:
#\Sigma(1-\delta)\delta^i=1
  f <- function(s) {
    sum((1 - s)* s^{0:260})
  }
#Solve the delta
res <- optim(1, f, lower = 0)
delta <- res$par
delta
#OK, now we find the delta is unrealistic, as delta cannot be zero.(Or to say delta^i almost zero)
#In this question, we assume delta is 0.2, as provided by the prof.

#Restore the data
stocks <- c("MMM","AXP","AAPL","BA","CAT","CVX","CSCO","KO","DIS","DWDP","XOM","GS","HD","IBM","INTC",
            "JNJ","JPM","MCD","MRK","MSFT","NKE","PFE","PG","TRV","UTX","UNH","VZ","V","WMT","WBA")
dj30_last5 = new.env()
#In this question, I use the data for last 5 years
getSymbols(stocks, src="yahoo", from="2014-01-01", to="2018-12-31")#last 5 years
returns_last5=c()
sigmat <- c()
#Load the monthly return data to return matrix. Yes, it's a return
for(stock in stocks){
  #stockAdjusted = dj30_last5[[stock]][,paste(stock, ".Adjusted",sep="")]
  #monthlyData = coredata(diff(log(apply.monthly(stockAdjusted, last))))
  #monthlyData=na.omit(monthlyData)
  monthlyData <- monthlyReturn(get(stock), type="log")
  returns_last5 <- cbind(returns_last5, monthlyData)
}
delta<-0.2
#for all 30 stocks, compute its sigmat
for(i in 1:30) {#iterate all 30 stocks
  the_square <- c()
  
  for(t in 13:nrow(returns_last5)) {#13 or 14?
    #change to 12
    
    #As the discussion from professor, we don't follow the formula on handout here, we use
    #12 \Sigma_(i=0)^11 (1-\delta)\delta^i (r_(t-i-1)-\bar(r))^2
    #The r_t-i-1, the rule return for that period
    r_t_i_1 = returns_last5[(t-1):(t-12),i]
    #bar_r, the mean for all r
    bar_r = sum((1 - delta)*delta^{0:11} * returns_last5[(t-1):(t-12),i])
    #The square is \Sigma^2_(s,t)
    the_square <- c(the_square, 12 * sum((1 - delta)*delta^{0:11}*( r_t_i_1 - bar_r)^2))
  }
  
  #the square is a square, so we need square root!
  sigmat <- cbind(sigmat, sqrt(the_square))
}
sigmat

#b-QUESTION 2 
#Predictive regression 
#Determine the optimal h for both 
#predictive regressions for all 30 DJ constituents
equation_left <- NULL

for(i in 1:30) {
  #As the formula: the one at the left of the equation is 
  #r_(s,t)/\sigma_(s,t-1)
  #We bind them together as matrix
  equation_left <- cbind(equation_left, returns_last5[13:nrow(returns_last5),i]/sigmat[,i])

}

#find optimal h for all 30 stocks
optimal_h_30 <- numeric(30)
for(i in 1:30) {
  
  rh <- numeric(12)
  for(h in 1:12) {
    #Actually there is a mapping between y and x
    #y is the equation left item
    #x is actually the sign of a rule return: (r_(s,t-h))
    #Still, make a matrix to calculate together
    #We start from h+1 because we start from h+1 th month using the previous h month data!
    y <- equation_left[(h+1):nrow(equation_left) ,i]
    x <- sign(returns_last5[(h+1):nrow(equation_left),i])
    #We try to fit the model
    model <- lm(y ~ x)
    #And then get the R_h
    rh[h] <- summary(model)$r.squared
  }
  #The optimal h is the one gets the highest R_h, isn't it?
  optimal_h_30[i]<- (1:12)[which.max(rh)]
}

#optimal h for 30 stocks with highest R-squared
#Add title for the result
names(optimal_h_30) <- stocks
optimal_h_30

#Question 3
#Summarize he performance
#TSMOM
TSMOM <- numeric(nrow(returns_last5) - 12)
#Start from the 13th month
for(i in 13:(nrow(returns_last5))) {
  #B_st = sign(return_(t-h:t)) * 40% / \sigma_t
  B_st <- numeric(30)
  for(j in 1:30) {
  #assume hs = 12 for all stocks
    B_st[j] <- sign(returns_last5[(i -12),j] )* 40/100 / sigmat[i-12,j]
  }
  #Still calculate TSMOM, with B * R. Remove the na
  TSMOM[i] <-  1/30 * sum(B_st * returns[i,], na.rm = TRUE)
}

#performances mean and vavriance of TSMOM portfolio
#You can see the mean is very small here
mean(TSMOM)
var(TSMOM)
perf_tsmom = ((12 * mean(TSMOM) - 0.02) / (sqrt(12) * sqrt(var(TSMOM))))
perf_tsmom


#PARTC
#Question changed:
#The same as the 1st question.
#The formula of ERT is in the discussion

#Similar with the ruleReturn function, however, the ER expression is changed.
ERh <- function(h, m, r, retX) {
  M<-length(d(m,r))-1
  acfs<- acf(retX, plot=F, type="covariance", lag.max=M)$acf
  #Actually the E(r_t) in formula
  mX<-mean(retX)
  ds = d(m,r)
  ER <- 0
  rXF = corXF(ds, retX)
  for (i in (1:length(ds))){
    #The sum of di * [r(i-j) - E(r_t)^2]
    ER <- ER + ds[i] * (rXF[m-r]- (mX^2))
  }
  #return ER
  ER

#2) Find optimal h=12 period holding period return
# for all 30 stocks

all_mr2 <- NULL

#Code from previous assignment, question 6/7
#Loop for each stock
for(stock in 1:30) {
  #We generate the current ERH, from the previous m and r data
  currERh = ERh(12, all_mr_double_ma[stock,1],all_mr_double_ma[stock,2],returns_last5[,stock])
  #Monthly return
  monthlyData <-monthlyReturn(get(stocks[stock]), type="log")
    #na.omit(ERh(12, all_mr_double_ma[stock,1],all_mr_double_ma[stock,2],returns_last5[,stock]))
  
  #choose the optimal m and r for monthly data
  result <- numeric(0)
  m <- numeric(0)
  r <- numeric(0)
  for (i in 2:11){
    for(j in (i+1):12){
      if(j>i){
        #We calculate the current ERh
        result <- c(result, ERh(12, m=j,r=i, monthlyData))
        m <- c(m,j)
        r <- c(r,i)
        
      }
    }
  }
  m_optimal <- m[which.max(result)]
  r_optimal <- r[which.max(result)]
  
  #combine them 
  all_mr2 <- rbind(all_mr2, c(m_optimal, r_optimal))
}

row.names(all_mr2) <- stocks
colnames(all_mr2) <- c("m", "r")
#optimal m and r 
all_mr2

