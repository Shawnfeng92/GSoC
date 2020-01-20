# ====
rm(list = ls())
library(PortfolioAnalytics)
library(Rglpk)
library(osqp)
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

pspec <- add.constraint(
  portfolio = pspec,
  type = "position_limit",
  max_pos = 5
)

pspec <- add.objective(pspec, type = "return", name = "mean")
# pspec <- add.objective(pspec, type = "risk", name = "ES")

Rglpk.result <- optimize.portfolio(
  R = returns, portfolio = pspec,
  optimize_method = "Rglpk", trace = FALSE,
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

mco.portfolio <- returns %*% mco.result$weights
pso.portfolio <- returns %*% pso.result$weights
Rglpk.portfolio <- returns %*% Rglpk.result$solution[1:13]

mean(mco.portfolio)
mean(Rglpk.portfolio)
mean(pso.portfolio)

ES(mco.portfolio)
ES(Rglpk.portfolio)
ES(pso.portfolio)

mean(pso.portfolio) / ES(pso.portfolio)
mean(Rglpk.portfolio) / ES(Rglpk.portfolio)
mean(mco.portfolio) / ES(mco.portfolio)