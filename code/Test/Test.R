# package ----
pack <- c("PortfolioAnalytics", "quadprog", "osqp", "Rglpk",
          "DEoptim", "foreach", "doSNOW", "doParallel")
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
# file ---- 
rm(list = ls())
source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")
# test functions ----
sharpetest <- function(x, sample) {
  time <- system.time(result1 <- optimize.portfolio(R = sample,
                                                   pspec, optimize_method = x,
                                                   verbos = 0))
  returns <- sample %*% result1$weights
  result <- c(x, round(c(time[3], median(returns)/sd(returns),
                         result1$weights),2))
  for (i in group_list) {
    result <- c(result, sum(result1$weights[i]))
  }
  result <- c(result, sum(result1$weights^2))
  result <- c(result, sum(result1$weights))
  names(result) <- c("method", "time", "Ratio", colnames(sample), "group1",
                     "group2", "group3", "group4", "SSum", "Sum")
  return(result)
}
CVaRtest <- function(x, sample) {
  time <- system.time(result <- optimize.portfolio(R = sample,
                                                   GSoC.CTA, optimize_method = x,
                                                   verbos = 0, alpha = 0.05))
  returns <- sample %*% result$weights
  result <- c(x, round(c(time[3], mean(returns), mean(returns[which(returns < quantile(returns, 0.05))]),
                         mean(returns)/mean(returns[which(returns < quantile(returns, 0.05))]),
                         result$weights),2))
  names(result) <- c("method", "time", "mean", "ES", "Ratio", colnames(sample))
  return(result)
}
# large data ----
data <- read.csv("~/GitHub/GSoC/data/fake.csv")
returns <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1]), format = "%Y-%m-%d"))[,sample(1:1500, 10)]
# CTA data ----
# data <- read.csv("~/GitHub/GSoC/data/.combined.csv")
# returns <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1]), format = "%m/%d/%Y"))
# simple portfolio ----
# GSoC.CTA <- portfolio.spec(assets = colnames(returns))
# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", 
#                            min_sum = 1, max_sum = 1)
# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
# group_list <- list(group1=c(1, 3, 5),
#                    group2=c(2, 4),
#                    groupA=c(2, 4, 5),
#                    groupB=c(1, 3))
# GSoC.CTA <- add.constraint(portfolio=GSoC.CTA, type="group",
#                         groups=group_list,
#                         group_min=c(0.15, 0.25, 0.2, 0.1),
#                         group_max=c(0.65, 0.55, 0.5, 0.4))
# GSoC.CTA <- add.constraint(GSoC.CTA, type ="position_limit", max_pos=7)
# GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
# GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")
# 
# result <- optimize.portfolio(returns, GSoC.CTA, optimize_method = "mco")
# w <- result$weights

# Complex Portfolio ----
pspec <- portfolio.spec(assets=colnames(returns))
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0.5, max_sum=1.05)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
# min <- c()
# max <- c()
# for (i in 1:20) {
#   temp <- runif(2,0,1)
#   min <- c(min, min(temp))
#   max <- c(max, max(temp))
# }
# pspec <- add.constraint(portfolio=pspec, type="box", min=min, max=max)
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
pspec <- add.objective(pspec, type = "return", name = "median")
pspec <- add.objective(pspec, type = "risk", name = "StdDev")
# result <- optimize.portfolio(returns, pspec, optimize_method = "mco")
# w <- result$weights

# Rglpk test ----
# methodsList <- c("DEoptim", "random", "pso", "GenSA", "Rglpk")
# cl <- makeCluster(16)
# registerDoSNOW(cl)
# iterations <- 5
# pb <- txtProgressBar(max = iterations, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# result <- foreach(i = 1:iterations, .combine = cbind, .options.snow = opts,
#                   .packages = c("Rglpk", "PortfolioAnalytics")) %dopar%
#   {
#     CVaRtest(methodsList[i], returns)
#   }
# close(pb)
# stopCluster(cl)
# osqp test ----
# methodsList <- c("DEoptim", "random", "pso", "GenSA", "osqp")
# cl <- makeCluster(16)
# registerDoSNOW(cl)
# iterations <- 5
# pb <- txtProgressBar(min = 0, max = iterations, style = 3)
# print("")
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# result <- foreach(i = 1:iterations, .combine = cbind, .options.snow = opts,
#                   .packages = c("osqp", "PortfolioAnalytics")) %dopar%
#   {
#     sharpetest(methodsList[i], returns)
#   }
# close(pb)
# stopCluster(cl)
# mco test ----
# methodsList <- c("DEoptim", "random", "pso", "GenSA", "mco")
# cl <- makeCluster(16)
# registerDoSNOW(cl)
# iterations <- 5
# pb <- txtProgressBar(max = iterations, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# result <- foreach(i = 1:iterations, .combine = cbind, .options.snow = opts,
#                   .packages = c("mco", "PortfolioAnalytics", "data.table")) %dopar%
#   {
#     sharpetest(methodsList[i], returns)
#   }
# close(pb)
# stopCluster(cl)

# mco test ----
# 
# f <- function(w) {
#   - mean(returns %*% w) / sd(returns %*% w)
# }
# 
# idim <- 12
# odim <- 1
# 
# g <- function(w) {
#   1 - sum(w)
# }
# 
# result <- nsga2(fn = f, idim = idim, odim = odim, cdim = 1,
#                 constraints = g, 
#                 lower.bounds = rep(0, 12), 
#                 upper.bounds = rep(1, 12), 
#                 popsize = 200, generations = 200)
# 
# round(result$par[1,], 2)

# mco test ----
methodsList <- c("DEoptim", "random", "pso", "GenSA", "mco")
cl <- makeCluster(16)
registerDoSNOW(cl)
iterations <- 5
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
result <- foreach(i = 1:iterations, .combine = cbind, .options.snow = opts,
                  .packages = c("mco", "PortfolioAnalytics")) %dopar%
  {
    sharpetest(methodsList[i], returns)
  }
close(pb)
stopCluster(cl)


















