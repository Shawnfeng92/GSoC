library(doParallel)
library(foreach)
library(quantmod)
library(Quandl)
library(gtools)

rm(list = ls()[ls()!="tickerList"])

Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")
letter <- chr(65:90)
tickerList <- letter

cl <- makeCluster(8)
registerDoParallel(cl)

# for (i in 1:3) {
#   tickerList <- foreach (j = tickerList, .combine = "c", .packages = "foreach")
#   %dopar% {
#     foreach (k = letter, .combine = "c")
#     %dopar% {
#       paste0(j,k)
#     }
#   }
# }
data <- c()
for (i in c("MMM", "AAA", "AAPL")) {
  a <- try(getSymbols(i, return.class = "xts", warnings = 0))
  if (class(a) != "try-error") {
    data <- rbind(data, a)
  }
}





data <- c()
stopCluster(cl)