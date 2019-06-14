library(doParallel)
library(foreach)
library(quantmod)
library(Quandl)
library(gtools)
library(rvest)
Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")

cl <- makeCluster(8)
registerDoParallel(cl)



stopCluster(cl)