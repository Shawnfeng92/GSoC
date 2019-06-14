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
tickerList <- tickerList[2:length(tickerList)]
tickerList <- as.character(tickerList)
tickerList <- tickerList[which(!"-" %in% tickerList)]

dataset <- foreach(i = tickerList, .combine = "cbind", .packages = "quantmod") %dopar% {
  getSymbols(i, source="yahoo", auto.assign=FALSE, return.class="xts")[,6]
}

stopCluster(cl)

rm(list = ls()[which(ls()!="dataset")])