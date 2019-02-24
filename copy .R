#copy partc 
#PARTC
#Question changed:
e_hperiod_holdperiodreturn <- function(retX, m, r, h) {
  acfs <- acf(retX, plot = F, type = "covariance", lag.max = m)$acf
  ER <- mean(retX)
  ehphpr <- h*sum(d(m,r)*(acfs[2:length(acfs)] + (ER^2)))
  ehphpr
}

#QUESTION 2
monthlyoptimalEHR_dma <- function(retX){
  optimal_m <- 2 
  optimal_r <- 1
  currEHR <- e_hperiod_holdperiodreturn(retX, optimal_m, optimal_r, 12)
  for (i in seq(1,11)){
    for (j in seq(i+1, 12)){
      EHRj <- e_hperiod_holdperiodreturn(retX, j, i, 12)
      if (EHRij > currEHR){
        optimal_m <- j
        optimal_r <- i
        currEHR <- EHRij
      }
    }
  }
  list("monthlyoptimal_m"=optimal_m, "monthlyoptimal_r"=optimal_r)
}

monthlyoptimalEHR_dma <- function(retsX){
  m <- c()
  r <- c()
  numstocks <- dim(retsX)[1]
  months <- dim(retsX)[2]
  for (i in seq(1, numstocks)){
    optimals <- monthlyoptimalEHR_dma(retsX[i, 1:months])
    m <- c(m, optimals$monthlyoptimal_m)
    r <- c(r, optimals$monthlyoptimal_r)
  }
  list("monthlyoptimals_m"=m, "monthlyoptimals_r"=r)
}
windowsmonthlyoptimalsEHR_dma <- function(retX){
  num_stocks <- dim(retX)[1]
  months <- dim(retX)[2]
  m <- matrix(,nrow = num_stocks, ncol= 1)
  r <- matrix(,nrow = num_stocks, ncol= 1)
  for (i in seq(1, months - 60, 12)){
    optimal_dma <- monthlyoptimalEHR_dma(
      retX[1:num_stocks, i:(i+60-1)])
    if(i==1){
      m[1:num_stocks, 1] <- optimal_dma$monthlyoptimals_m
      r[1:num_stocks, 1] <- optimal_dma$monthlyoptimals_r
    }else{
      m <- cbind(m, optimal_dma$monthlyoptimals_m)
      r <- cbind(r, optimal_dma$monthlyoptimals_r)
    }
    
    
  }
  list("m"=m, "r"=r)
}
