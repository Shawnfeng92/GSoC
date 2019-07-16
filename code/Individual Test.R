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

source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")
data <- read.csv("~/GitHub/GSoC/data/.combined.csv")
returns <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1])))

ES <- function(x) {
  return(mean(x[which(x<quantile(x,0.05))]))
}
    
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

Rglpk.result <- optimize.portfolio(R = returns, portfolio = pspec, optimize_method = "Rglpk", verbose = FALSE)

pso.result <- optimize.portfolio(R = returns, portfolio = pspec, optimize_method = "pso")

Rglpk.w <- Rglpk.result$weights
pso.w <- pso.result$weights

Rglpk.out <- mean(Rglpk.w)/ES(returns %*% Rglpk.w)
pso.out <- mean(pso.w)/ES(returns %*% pso.w)

print(Rglpk.out)
print(pso.out)

print(sum(Rglpk.w))
print(sum(pso.w))

sapply(group_list, function(x){return(sum(Rglpk.w[x]))})
sapply(group_list, function(x){return(sum(pso.w[x]))})






