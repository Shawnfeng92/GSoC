library(doParallel)
library(foreach)
library(quantmod)
library(Quandl)
library(gtools)

Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")
letter <- chr(65:90)
tickerList <- letter

cl <- makeCluster(8)
registerDoParallel(cl)

for (i in 1:4) {
  temp <- c()
  foreach (j = tickerList) %dopar% {
    for (k in letter) {
      temp <- c(temp, paste0(j,k))
    }
  }
  tickerList <- temp
}


data <- c()









































stopCluster(cl)