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