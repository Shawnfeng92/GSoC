# ====
rm(list = ls())
library(PortfolioAnalytics)
library(Rglpk)
library(osqp)
library(mco)
library(doSNOW)
library(doParallel)

source(file = "optimize.portfolio.R")

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

pspec <- add.constraint(
  pspec, "position_limitation",
  max_pos = 5
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
  rtime <- system.time(
    weight <- optimize.portfolio(
      R = returns, portfolio = pspec,
      optimize_method = i, trace = FALSE,
      message = FALSE
    )$weights
  )

  portfolio <- returns %*% weight
  p.ES <- ES(portfolio)
  p.sd <- sd(portfolio)
  p.mean <- mean(portfolio)
  r <- c(
    p.mean, p.sd, p.ES, p.mean / p.sd, p.mean / p.ES,
    rtime["elapsed"], weight
  )
  r
}
stopImplicitCluster()

rownames(results) <- solvers
colnames(results) <- c(
  "mean", "sd", "ES", "Sharpe",
  "STARR", "time", colnames(returns)
)

round(results, 2)