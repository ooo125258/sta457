#PART A 刘畅 
returns <- NULL
for(stock in stocks) {
  
  monthlyData <- monthlyReturn(get(stock), type="log")
  
  returns <- cbind(returns, monthlyData)
  
}

#EW
Br <- numeric(nrow(returns))
for(i in 1:nrow(returns)) {
  Br[i] <- mean(returns[i,],na.rm = TRUE)
}
mean(Br)
var(Br)

#RP
Rp <- numeric(nrow(returns))
sds <-apply(returns,2,sd, na.rm = TRUE)
ws <- (1/sds)/(sum(1/sds))
for(i in 1:nrow(returns)) {
  Rp[i] <- sum(returns[i,] * ws, na.rm = TRUE)
}

mean(Rp)
var(Rp)

mean(RP)
var(RP)

perf_ew1 = ((12 * mean(EW) - 0.02) / (sqrt(12) * sqrt(var(EW))))
perf_rp1 = ((12 * mean(RP) - 0.02) / (sqrt(12) * sqrt(var(RP))))
perf_ew1
perf_rp1

#OBSERVATION AND EXPLANATION:
#performance of equally weighted portfolio has a value of 0.4157, which is greater than delta= 0.02; 
#furthermore, the risk parity portfolio has less volatility than the equally weighted portfolio. Due to
#the fact that risk parity portfolio is well-diversified and it has low idiosyncratic risk than equally weighted
#portfolio where market risk still exists. this is expected as risk parity seeks equal risk exposure from all assets 
#in the portfolio, therefore, less weight is allocated to more risky asstes in comparison to equally weighted portfolio 
#where equal weight of each asset is required.
