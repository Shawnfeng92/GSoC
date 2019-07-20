{
  library(doSNOW)
  library(foreach)
  library(quantmod)
  library(Quandl)
}

dataset <- function(filelist = c("AMEX", "NYSE", "NASDAQ")){
  Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")
  cl <- makeCluster(8)
  registerDoSNOW(cl)
  
  tickers <- foreach(i = filelist, .combine = "rbind") %dopar% {
    read.delim(file = paste0("~/GitHub/GSoC/data/Tickers/", i, ".txt"), 
               stringsAsFactors = FALSE)
  }
  tickers <- as.character(tickers[,1])
  
  prices <- foreach(i = tickers, .combine = "cbind", .packages = "quantmod") %dopar% {
    temp <- try(getSymbols(i, source="yahoo", auto.assign=FALSE, return.class="xts")[,6])
    if (class(temp) != "try-error") {
      colnames(temp) <- c(i)
      temp 
    }
  }
  stopCluster(cl)
  returns <- diff(log(prices)) 
  return(list(Price = prices,
              Return = returns[-1,]))
}

#data <- dataset()

checkStop <- function(x) {
  exist <- 0
  period <- 0
  for (i in length(x):1) {
    if (!is.null(x[i])) {
      exist <- 1
      period <- 0
    } 
    else {
      period <- period + 1
    }
    if (exist & (period >= 7)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

checkTradable <- function(x) {
  
}

inequal <- function(x,y) {
  existdate <- which(!is.na(x+y))
  cov(x[existdate], y[existdate])
}

simulateCovPar <- function(x) {
  inequal <- function(x,y) {
    existdate <- which(!is.na(x+y))
    cov(x[existdate], y[existdate])
  }
  cl <- makeCluster(8)
  registerDoSNOW(cl)
  result <- foreach(i = 1:ncol(x), .combine = "rbind", .packages = c("doSNOW", "foreach")) %dopar% {
    foreach(j = 1:ncol(x), .combine = "c") %dopar% {
      inequal(x[,i], x[,j])
    }
  }
  stopCluster(cl)
  colnames(result) <- rownames(result) <- colnames(x)
  return(result)
}

simulateCovSingle <- function(x) {
  result <- c()
  for (i in 1:ncol(x)) {
    temp<- c()
    for (j in 1:ncol(x)) {
      temp <- c(temp, inequal(x[,i], x[,j]))
    }
    result <- rbind(result, temp)
  }
  return(result)
}

BFM <- function(N = 100, T = 2500){
  result <- matrix(rep(NA, N*T), nrow = T, ncol = N)
  for (i in 1:N) {
    x <-sample(1:T,1) 
    result[x:T,i] <- round(rnorm(length(x:T)), 2)
  }
  return(result)
}

testTime <- function(N = 100, T = 2500)
{
  print(system.time(simulateCovPar(BFM(N, T))))
  print(system.time(simulateCovSingle(BFM(N, T))))
}

testTime()



