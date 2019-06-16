library(PortfolioAnalytics)
library(quadprog)
library(osqp)
library(Rglpk)
library(DEoptim)
library(foreach)
library(doSNOW)

rm(list = ls())
source("~/Documents/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")

data <- read.csv("~/Documents/GitHub/GSoC/data/fake.csv")
data <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1]), format = "%Y-%m-%d"))
testdata <- data[,sample(1:1500, 20)]

GSoC.CTA <- portfolio.spec(assets = colnames(testdata))
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", min_sum = -1, max_sum = 1)
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
group_list <- list(group1=c(1, 3, 5),
                   group2=c(2, 4),
                   groupA=c(2, 4, 5),
                   groupB=c(1, 3))
GSoC.CTA <- add.constraint(portfolio=GSoC.CTA, type="group",
                        groups=group_list,
                        group_min=c(0.15, 0.25, 0.2, 0.1),
                        group_max=c(0.65, 0.55, 0.5, 0.4))

GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")

methodsList <- c("DEoptim", "random", "pso", "GenSA", "osqp")

sharpetest <- function(x, sample) {
  time <- system.time(result <- optimize.portfolio(R = sample, GSoC.CTA, optimize_method = x, verbos = 0))
  returns <- sample %*% result$weights
  result <- c(x, time[3], mean(returns)/sd(returns), result$weights)
  names(result) <- c("method", "time", "Sharpe", colnames(sample))
  round(result, 2)
}

cl <- makeCluster(8)
registerDoParallel(cl)
iterations <- 5
f <- function(){
  pb <- txtProgressBar(min=1, max=4,style=3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb,count)
    flush.console()
    c(...)
  }
}
result <- foreach(i = icount(5), .combine = f(),
                  .packages = c("osqp", "PortfolioAnalytics")) %dopar%
  {
    sharpetest(methodsList[i], testdata)
  }
close(pb)
stopCluster(cl) 






