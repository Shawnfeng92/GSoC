library(doParallel)
library(foreach)
library(quantmod)
library(Quandl)
library(gtools)
library(rvest)
Quandl.api_key("dU-ukkHjcYwUsDqmcvjB")

cl <- makeCluster(8)
registerDoParallel(cl)

tickerList <- foreach (i = chr(65:90), .combine = "c", .packages = "rvest") %dopar% {
  url <- paste0("http://eoddata.com/stocklist/NYSE/", i, ".htm")
  webpage <- read_html(url)
  ticker_html <- html_nodes(webpage,'a')
  ticker <- html_text(ticker_html)
  ticker <- ticker[18:length(ticker)]
  ticker <- ticker[which(ticker != "")][26:length(ticker)]
  ticker <- ticker[1:(which(ticker == "Register")-1)]
}
stopCluster(cl)
rm(list = ls()[-which(ls()=="tickerList")])