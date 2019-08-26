{ 
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
}

rm(list = ls())

source("~/GitHub/GSoC/Final Evaluation/optimize.portfolio.R")
data <- read.csv("~/GitHub/GSoC/data/Returns/.combined.csv")
returns <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1]), format = "%m/%d/%Y"))


ES <- function(x) {
  return(mean(x[which(x<quantile(x,0.05))]))
}

# comparison test on Rglpk
# linear programming problems with mean return reward and ES risk

pspec <- portfolio.spec(assets=colnames(returns))

pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "ES")

pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1.1)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
group_list <- list(group1=c(1, 3, 5),
                   group2=c(2, 4),
                   groupA=c(2, 4, 5),
                   groupB=c(1, 3))
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=group_list,
                        group_min=c(0.15, 0.25, 0.2, 0.1),
                        group_max=c(0.65, 0.55, 0.5, 0.4))
lRglpk <- optimize.portfolio(returns, pspec, optimize_method = "Rglpk", sil = 1)
lGenSA <- optimize.portfolio(returns, pspec, optimize_method = "GenSA")
lpso <- optimize.portfolio(returns, pspec, optimize_method = "pso")
lDEoptim <- optimize.portfolio(returns, pspec, optimize_method = "DEoptim")
lrandom <- optimize.portfolio(returns, pspec, optimize_method = "random")

rRglpk <- 


# comparison test on osqp
# quadratic programming problems with mean return reward and volatility risk

pspec <- portfolio.spec(assets=colnames(returns))

pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "StdDev")

pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1.1)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
group_list <- list(group1=c(1, 3, 5),
                   group2=c(2, 4),
                   groupA=c(2, 4, 5),
                   groupB=c(1, 3))
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=group_list,
                        group_min=c(0.15, 0.25, 0.2, 0.1),
                        group_max=c(0.65, 0.55, 0.5, 0.4))

qosqp <- optimize.portfolio(returns, pspec, optimize_method = "osqp", sil = 1)
qGenSA <- optimize.portfolio(returns, pspec, optimize_method = "GenSA")
qpso <- optimize.portfolio(returns, pspec, optimize_method = "pso")
qDEoptim <- optimize.portfolio(returns, pspec, optimize_method = "DEoptim")
qrandom <- optimize.portfolio(returns, pspec, optimize_method = "random")

# comparison test on mco
# general convex optimize problems with mean return reward and volatility risk

pspec <- portfolio.spec(assets=colnames(returns))

pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "StdDev")

pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1.1)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
group_list <- list(group1=c(1, 3, 5),
                   group2=c(2, 4),
                   groupA=c(2, 4, 5),
                   groupB=c(1, 3))
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=group_list,
                        group_min=c(0.15, 0.25, 0.2, 0.1),
                        group_max=c(0.65, 0.55, 0.5, 0.4))
pspec <- add.constraint(portfolio = pspec, type = "position_limit", max_pos = 6)
pmco <- optimize.portfolio(returns, pspec, optimize_method = "mco", sil = 1)
pGenSA <- optimize.portfolio(returns, pspec, optimize_method = "GenSA")
ppso <- optimize.portfolio(returns, pspec, optimize_method = "pso")
pDEoptim <- optimize.portfolio(returns, pspec, optimize_method = "DEoptim")
prandom <- optimize.portfolio(returns, pspec, optimize_method = "random")











