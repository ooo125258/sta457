#PARTC
#Question changed:
#Question 1 
ERh <- function(h, m, retX) {
  rth <- c()
  for(t in (h+1):(nrow(retX) - h)) {
    s <- 0 
    for(i in 0:(h-1)) {
      
      j <- 0:(m-2)
      #os.numeric ??????????????
      s <- s + as.numeric(sum(retX[t - j + i - 1]) * retX[t + i])
    }
    rth[t] <- s
  }
  rth
}

#2) Find optimal h=12 period holding period return
# for all 30 stocks

all_mr2 <- NULL

for(stock in 1:30) {
  monthlyData <- na.omit(ERh(12, 3,returns[,stock]))
  
  #choose the optimal m and r for monthly data
  result <- numeric(0)
  m <- numeric(0)
  r <- numeric(0)
  for (i in 2:51){
    for(j in (i+1):52){
      if(j>i){
        result <- c(result.ruleReturn(monthlyData, m=j, r = i)[1])
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
columes(all_mr2) <- c("m", "r")
#optimal m and r 
all_mr2