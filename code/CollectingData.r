library(doParallel)
library(foreach)
library(quantmod)
library(Quandl)
library(gtools)

Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")

letter <- chr(65:90)

tickerList <- letter

for (i in 1:3) {
  temp <- c()
  for (j in tickerList) {
    for (k in letter) {
      temp <- c(temp, paste0(j,k))
    }
  }
  tickerList <- temp
}

cl <- makeCluster(8)
registerDoParallel(cl)

data <- c()









































stopCluster(cl)