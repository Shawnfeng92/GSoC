# ====
rm(list = ls())
library(PortfolioAnalytics)
library(Rglpk)
library(osqp)
library(mco)
library(doSNOW)
library(doParallel)

source(file = "GitHub/GSoC/code/Test/optimize.portfolio.R")

data(edhec)
returns <- edhec

rm(
  edhec,
  optimize.portfolio.rebalancing_v1,
  optimize.portfolio.parallel,
  optimize.portfolio.rebalancing,
  optimize.portfolio_v1
)

ES <- function(d) {
  VaR <- quantile(d, 0.05)
  return(mean(d[which(d <= VaR)]))
}

# ====
pspec <- portfolio.spec(assets = colnames(returns))
pspec <- add.constraint(
  portfolio = pspec, type = "weight_sum",
  min_sum = -0.5, max_sum = 1
)

pspec <- add.constraint(
  portfolio = pspec,
  type = "box",
  min = rep(-0.2, 13),
  max = 1:13 / 13 * 2
)

pspec <- add.constraint(
  portfolio = pspec,
  type = "group",
  groups = list(
    c(1, 2, 3),
    c(4, 5, 6)
  ),
  group_min = c(0, 0.1),
  group_max = c(0.5, 0.6)
)

# pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "ES")

solvers <- c("Rglpk", "mco", "GenSA", "DEoptim", "pso", "random")

registerDoParallel(16)

results <- foreach(
  i = solvers, .combine = "rbind",
  .packages = c(
    "PortfolioAnalytics",
    "osqp", "Rglpk", "mco"
  )
) %dopar% {
  optimize.portfolio(
    R = returns, portfolio = pspec,
    optimize_method = i, trace = FALSE,
    message = FALSE
  )$weights
}
stopImplicitCluster()

rownames(results) <- solvers

for (i in 1:6) {
  print(PortfolioAnalytics:::check_constraints(results[i,], pspec))
  t <- returns %*% results[i,]  
  print(ES(t))
  rm(t)
}