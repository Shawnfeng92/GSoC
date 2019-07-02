library(PortfolioAnalytics)
library(quadprog)
library(osqp)
library(Rglpk)
library(DEoptim)
library(foreach)
library(doSNOW)
library(mco)
library(data.table)
library(GenSA)
library(pso)
library(Quandl)

rm(list = ls())

source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")

test <- function(method_list = c("DEoptim", "random", "pso", "GenSA", "mco"), risk = "StdDev", reward = "mean", returns = "CTA", num = 20) {
  # package ----
  pack <- c(method_list, "PortfolioAnalytics")
  pack <- pack[which(pack != "random")]
  
  # data ----
  if (returns == "CTA") {
    data <- read.csv("~/GitHub/GSoC/data/.combined.csv")
    returns <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1]), format = "%m/%d/%Y"))
  } else {
    data <- read.csv("~/GitHub/GSoC/data/fake.csv")
    returns <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1]), format = "%Y-%m-%d"))[,sample(1:1500, num)]
  }
  
  # portfolio ----
  pspec <- portfolio.spec(assets=colnames(returns))
  pspec <- add.objective(pspec, type = "return", name = reward)
  pspec <- add.objective(pspec, type = "risk", name = risk)

  pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0.5, max_sum=1.05)
  pspec <- add.constraint(portfolio = pspec, type = "long_only")
  group_list <- list(group1=c(1, 3, 5),
                     group2=c(2, 4),
                     groupA=c(2, 4, 5),
                     groupB=c(1, 3))
  pspec <- add.constraint(portfolio=pspec, type="group",
                          groups=group_list,
                          group_min=c(0.15, 0.25, 0.2, 0.1),
                          group_max=c(0.65, 0.55, 0.5, 0.4))
  pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)
  pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)
  # pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.2)
  # pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.007)
  
  # test ----
  cl <- makeCluster(16)
  registerDoSNOW(cl)
  iterations <- length(method_list)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  result <- foreach(i = 1:iterations, .combine = rbind, .options.snow = opts, .packages = pack) %dopar%
    {
      rtime <- system.time(
        weight <- optimize.portfolio(R = returns, portfolio = pspec, optimize_method = method_list[i])$weights
      )
      print(weight)
      result <- c(weight, rtime[1])
      names(result) <- c(colnames(returns), "Running Time")
      result
    }
  rownames(result) <- method_list
  close(pb)
  stopCluster(cl)
  
  weights <- result[, 1:ncol(returns)]
  
  portfolios <- apply(weights, 1, function(w){returns %*% w})
  
  if (reward == "mean") {
    up <- apply(portfolios, 2, mean)
  } else if (reward == "median") {
    up <- apply(portfolios, 2, median)
  }
  
  if (risk == "StdDev") {
    down <- apply(portfolios, 2, sd)
  } else if (risk == "VaR") {
    down <- apply(portfolios, 2, function(R){quantile(R, 0.05)})
  } else if (risk %in% c("ES", "AVaR", "CVaR")) {
    down <- apply(portfolios, 2, function(R){mean(R[which(R < quantile(R, 0.05))])})
  }
  
  list(weight = weights*100,
       statistic = rbind(up/down * 100, 
                         result[, ncol(returns) + 1], 
                         sum = apply(weights, 1, sum)))
}
test()