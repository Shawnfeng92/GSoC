library(PortfolioAnalytics, PerformanceAnalytics)
library(zoo, xts)
library(foreach, rootSolve)
library(quantmod, DEoptim)
library(fGarch, iterators)
library(Rglpk, ROI)
library(ROI.plugin.quadprog, ROI.plugin.symphony)
library(ROI.plugin.glpk, quadprog)
library(GenSA, pso)
library(corpcor, testthat)
library(nloptr, ggplot2)
library(MASS, robustbase)
library(mice)
library(quadprog)
library(osqp)

# Data
test <- function(method){
  result <- read.csv("~/GitHub/GSoC/data/.combined.csv")
  result <- xts(result[,2:13], order.by = as.Date(as.character(result[,1]), format = "%m/%d/%Y"))
  
  source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")
  
  CTAs <- colnames(result)
  GSoC.CTA <- portfolio.spec(assets = CTAs)
  
  GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", min_sum = 0.95, max_sum = 1.05)
  GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
  # GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "turnover", turnover_target = 0.5)
  # GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "diversification", div_target = 0.7)
  # GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "position__limit", 6)
  # GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "return", return_target = 0.02)
  # GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "leverage_exposure", leverage=1.6)
  
  GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
  GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")
  
  start <- Sys.time()
  final <- optimize.portfolio(R = result, GSoC.CTA, optimize_method = method, verbos = 0)
  start <- Sys.time() - start
  p <- result %*% final$weights
  
  print(method)
  print(paste0("Sharpe Ratio = ",round(mean(p) / sd(p),2)))
  print(paste0("sum = ", sum(final$weights)))
  print(start)
}

mlist <- c("DEoptim", "random", "pso", "GenSA", "osqp")

for (i in mlist) {
  test(i)
}

