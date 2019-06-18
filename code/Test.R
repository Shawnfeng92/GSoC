pack <- c("PortfolioAnalytics", "quadprog", "osqp", "Rglpk",
          "DEoptim", "foreach", "doSNOW", "doParallel")
library(PortfolioAnalytics)
library(quadprog)
library(osqp)
library(Rglpk)
library(DEoptim)
library(foreach)
library(doSNOW)

rm(list = ls())
source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")

data <- read.csv("~/GitHub/GSoC/data/fake.csv")
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
                                                   verbos = 0))
  returns <- sample %*% result$weights
  result <- c(x, round(c(time[3], mean(returns)/sd(returns),  
                         result$weights),2))
  names(result) <- c("method", "time", "Ratio", colnames(sample))
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

cl <- makeCluster(16)
registerDoSNOW(cl)
iterations <- 5
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
result <- foreach(i = 1:iterations, .combine = cbind, .options.snow = opts,
                  .packages = c("Rglpk", "PortfolioAnalytics")) %dopar%
  {
    CVaRtest(methodsList[i], testdata)
  }
close(pb)
stopCluster(cl) 

cl <- makeCluster(16)
registerDoSNOW(cl)
iterations <- 1600
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
result <- foreach(i = 1:iterations, .combine = cbind, .options.snow = opts,
                  .packages = c("Rglpk", "PortfolioAnalytics")) %dopar%
  {
    CVaRtest(methodsList[5], testdata)
  }
close(pb)
stopCluster(cl) 

data(edhec)
returns <- edhec[, 1:4]
fund.names <- colnames(returns)
pspec <- portfolio.spec(assets=fund.names)
# Add the full investment constraint that specifies the weights must sum to 1.
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1)
# The full investment constraint can also be specified with type="full_investment"
pspec <- add.constraint(portfolio=pspec, type="full_investment")
# Another common constraint is that portfolio weights sum to 0.
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0, max_sum=0)
pspec <- add.constraint(portfolio=pspec, type="dollar_neutral")
pspec <- add.constraint(portfolio=pspec, type="active")
# Add box constraints
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.4)
# min and max can also be specified per asset
pspec <- add.constraint(portfolio=pspec,
                        type="box",
                        min=c(0.05, 0, 0.08, 0.1),
                        max=c(0.4, 0.3, 0.7, 0.55))
# A special case of box constraints is long only where min=0 and max=1
# The default action is long only if min and max are not specified
pspec <- add.constraint(portfolio=pspec, type="box")
pspec <- add.constraint(portfolio=pspec, type="long_only")
# Add group constraints
pspec <- add.constraint(portfolio=pspec,
                        type="group",
                        groups=list(c(1, 2, 1), 4),
                        group_min=c(0.1, 0.15),
                        group_max=c(0.85, 0.55),
                        group_labels=c("GroupA", "GroupB"),
                        group_pos=c(2, 1))
# Add position limit constraint such that we have a maximum number
# of three assets with non-zero weights.
pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)
# Add diversification constraint
pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)
# Add turnover constraint
pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.2)
# Add target mean return constraint
pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.007)
# Example using the indexnum argument
portf <- portfolio.spec(assets=fund.names)
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")
# indexnum corresponds to the index number of the constraint
# The full_investment constraint was the first constraint added and has
# indexnum=1
portf$constraints[[1]]
# View the constraint with indexnum=2
portf$constraints[[2]]
# Update the constraint to relax the sum of weights constraint
portf <- add.constraint(portf, type="weight_sum",
                        min_sum=0.99, max_sum=1.01,
                        indexnum=1)
# Update the constraint to modify the box constraint
portf <- add.constraint(portf, type="box",
                        min=0.1, max=0.8,
                        indexnum=2)




