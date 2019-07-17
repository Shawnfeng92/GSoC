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

data <- dataset()

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