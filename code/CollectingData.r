library(doParallel)
library(foreach)
library(quantmod)
library(Quandl)
library(gtools)
Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")
rm(list = ls())

cl <- makeCluster(8)
registerDoParallel(cl)

tickerList <- read.csv(file = "~/Documents/GitHub/GSoC/data/tickers.csv",header = 0)[,1]
tickerList <- as.character(tickerList)

data <- c()
for (i in c("MMM", "AAA", "AAPL")) {
  a <- getSymbols(i, return.class = "xts", warnings = 0, auto.assign = 1)
  if (class(a) != "try-error") {
    data <- rbind(data, a)
  }
}





data <- c()
stopCluster(cl)