library(doParallel)
library(foreach)
library(quantmod)
library(Quandl)
library(gtools)
library(rvest)
Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")
cl <- makeCluster(8)
registerDoParallel(cl)
tickerList <- read.csv("~/Documents/GitHub/GSoC/data/tickers.csv")[,2]
tickerList <- as.character(tickerList)
tickerList <- tickerList[!grepl("-", tickerList)]
tickerList <- tickerList[!grepl(".", tickerList, fixed = 1)]
time <- system.time(
dataset <- foreach(i = tickerList, .combine = "cbind", .packages = "quantmod") %dopar% {
  temp <- try(getSymbols(i, source="yahoo", auto.assign=FALSE, return.class="xts")[,6])
  if (class(A) != "try-error") {
    colnames(temp) <- c(i)
    temp 
  } else {
    return()
  }
})
stopCluster(cl)
rm(list = ls()[which(ls()!="dataset")])