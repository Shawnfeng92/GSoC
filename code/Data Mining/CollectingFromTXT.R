{
  library(doSNOW)
  library(foreach)
  library(quantmod)
  library(Quandl)
}

Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")
cl <- makeCluster(8)
registerDoSNOW(cl)

tickers <- rbind(
  read.delim(file = "~/GitHub/GSoC/data/Tickers/AMEX.txt"),
  read.delim(file = "~/GitHub/GSoC/data/Tickers/NASDAQ.txt"),
  read.delim(file = "~/GitHub/GSoC/data/Tickers/NYSE.txt"))[,1]

dataset <- foreach(i = tickers, .combine = "cbind", .packages = "quantmod") %dopar% {
  temp <- try(getSymbols(i, source="yahoo", auto.assign=FALSE, return.class="xts")[,6])
  if (class(temp) != "try-error") {
    colnames(temp) <- c(i)
    temp 
  } else {
    temp <- NA
  }
}

stopCluster(cl)

