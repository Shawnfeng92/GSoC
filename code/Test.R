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
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "CVaR")

methodsList <- c("DEoptim", "random", "pso", "GenSA", "Rglpk")

sharpetest <- function(x, sample) {
  time <- system.time(result <- optimize.portfolio(R = sample, 
                                                   GSoC.CTA, optimize_method = x, 
                                                   verbos = 0, alpha = 0.05))
  returns <- sample %*% result$weights
  result <- c(x, round(c(time[3], 
                         mean(returns)/mean(returns[which(returns < quantile(returns, 0.05))]), 
                         result$weights),2))
  names(result) <- c("method", "time", "Sharpe", colnames(sample))
  return(result)
}

cl <- makeCluster(8)
registerDoSNOW(cl)
iterations <- 5
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
result <- foreach(i = 1:iterations, .combine = cbind, .options.snow = opts,
                  .packages = c("Rglpk", "PortfolioAnalytics")) %dopar%
  {
    sharpetest(methodsList[i], testdata)
  }
close(pb)
stopCluster(cl) 





