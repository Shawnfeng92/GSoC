# ====

library(PortfolioAnalytics)
library(osqp)
library(Rglpk)
library(mco)

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

# ====
pspec <- portfolio.spec(assets = colnames(returns))
pspec <- add.constraint(
  portfolio = pspec, type = "weight_sum",
  min_sum = -1, max_sum = 1
)

pspec <- add.constraint(
  portfolio = pspec,
  type = "box",
  min = rep(-0.5, 13),
  max = 1:13/10
)

pspec <- add.constraint(
  portfolio = pspec,
  type = "group",
  groups = list(
    c(1, 2, 3),
    c(2, 3, 4)
  ),
  group_min = c(0, 0.1),
  group_max = c(0.5, 0.6)
)

pspec <- add.constraint(
  portfolio = pspec,
  type = "return",
  return_target = 0.05
)

# pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "StdDev")

osqp.result <- optimize.portfolio(
  R = returns, portfolio = pspec,
  optimize_method = "osqp", trace = FALSE,
  message = FALSE
)

mco.result <- optimize.portfolio(
  R = returns, portfolio = pspec,
  optimize_method = "mco", trace = FALSE,
  message = FALSE
)

GenSA.result <- optimize.portfolio(
  R = returns, portfolio = pspec,
  optimize_method = "GenSA", trace = FALSE,
  message = FALSE
)

PortfolioAnalytics:::check_constraints(osqp.result$x, pspec)

sqrt(t(mco.result$par[1,]) %*% cov(returns) %*% mco.result$par[1,])
sqrt(t(osqp.result$x) %*% cov(returns) %*% osqp.result$x)
sqrt(t(GenSA.result$weights) %*% cov(returns) %*% GenSA.result$weights)
