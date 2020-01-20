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
  min_sum = -0.5, max_sum = 1
)

pspec <- add.constraint(
  portfolio = pspec,
  type = "box",
  min = rep(-0.1, 13),
  max = 1:13/10
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

# pspec <- add.constraint(
#   portfolio = pspec,
#   type = "return",
#   return_target = 0.05
# )

pspec <- add.objective(pspec, type = "return", name = "mean")
# pspec <- add.objective(pspec, type = "risk", name = "StdDev")

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

pso.result <- optimize.portfolio(
  R = returns, portfolio = pspec,
  optimize_method = "pso", trace = FALSE,
  message = FALSE
)

sqrt(t(mco.result$weights) %*% cov(returns) %*% mco.result$weights)
sqrt(t(pso.result$weights) %*% cov(returns) %*% pso.result$weights)
sqrt(t(osqp.result$x) %*% cov(returns) %*% osqp.result$x)
