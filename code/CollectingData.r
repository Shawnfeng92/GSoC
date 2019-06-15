library(doParallel)
library(foreach)
library(quantmod)
library(Quandl)
library(gtools)
library(rvest)

# data collection ----
collect <- function(){
  Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")
  cl <- makeCluster(8)
  registerDoParallel(cl)
  time1 <- system.time(
    stocklist <- foreach(x = chr(65:90), .combine = "c", .packages = "rvest") %dopar%
    {
      url <- paste0('http://eoddata.com/stocklist/NYSE/', x, '.htm')
      webpage <- read_html(url)
      stockhtml <- html_nodes(webpage,'a')
      stock <- html_text(stockhtml)
      stock <- stock[43:length(stock)]
      stock <- stock[1:(which(stock == "Register")-1)]
      stock <- stock[which(stock != "")]
      stock <- stock[!grepl("-", stock)]
      stock <- stock[!grepl(".", stock, fixed = 1)]
    }
  )
  write.csv(stocklist,"Documents/GitHub/GSoC/data/tickers.csv")
  time2 <- system.time(
    dataset <- foreach(i = stocklist, .combine = "cbind", .packages = "quantmod") %dopar% {
      temp <- try(getSymbols(i, source="yahoo", auto.assign=FALSE, return.class="xts")[,6])
      if (class(temp) != "try-error") {
        colnames(temp) <- c(i)
        temp 
      } else {
        temp <- NA
      }
    }
  )
  stopCluster(cl)
  write.zoo(dataset, "Documents/GitHub/GSoC/data/all.csv", sep = ",")
}
# choose suitable data ----
dataset <- read.csv(file = "~/Documents/GitHub/GSoC/data/all.csv")
dataset <- xts(dataset[,2:ncol(dataset)], as.Date(dataset[,1], "%m/%d/%y"))