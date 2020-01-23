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

# LP optimization ====
# asset information
pspec.ES <- portfolio.spec(assets = colnames(returns))

# constraints: leverage, box, group
pspec.ES <- add.constraint(
  portfolio = pspec.ES, type = "weight_sum",
  min_sum = -0.5, max_sum = 1
)

pspec.ES <- add.constraint(
  portfolio = pspec.ES,
  type = "box",
  min = rep(-0.2, 13),
  max = 1:13 / 13 * 2
)

pspec.ES <- add.constraint(
  portfolio = pspec.ES,
  type = "group",
  groups = list(
    c(1, 2, 3),
    c(4, 5, 6)
  ),
  group_min = c(0, 0.1),
  group_max = c(0.5, 0.6)
)

# objectives: mean as return, ES as risk
pspec.ES <- add.objective(pspec.ES, type = "return", name = "mean")
pspec.ES <- add.objective(pspec.ES, type = "risk", name = "ES")

# linear programming solvers
LP.solvers <- c("Rglpk", "mco", "GenSA", "DEoptim", "pso", "random")

# parallel solution
registerDoParallel(8)

LP.results <- foreach(
  i = LP.solvers, .combine = "rbind",
  .packages = c(
    "PortfolioAnalytics",
    "osqp", "Rglpk", "mco"
  )
) %dopar% {
  rtime <- system.time(
    weight <- optimize.portfolio(
      R = returns, portfolio = pspec.ES,
      optimize_method = i, trace = FALSE,
      message = FALSE
    )$weights
  )

  portfolio <- returns %*% weight
  p.ES <- ES(portfolio)
  p.mean <- mean(portfolio)
  r <- c(
    p.mean, p.ES, p.mean / p.ES,
    rtime["elapsed"], weight
  )
  r
}
stopImplicitCluster()

# results style
rownames(LP.results) <- LP.solvers
colnames(LP.results) <- c(
  "mean", "ES",
  "STARR", "time", colnames(returns)
)

# QP optimization ====
# asset information
pspec.sd <- portfolio.spec(assets = colnames(returns))

# constraints: leverage, box, group
pspec.sd <- add.constraint(
  portfolio = pspec.sd, type = "weight_sum",
  min_sum = -0.5, max_sum = 1
)

pspec.sd <- add.constraint(
  portfolio = pspec.sd,
  type = "box",
  min = rep(-0.2, 13),
  max = 1:13 / 13 * 2
)

pspec.sd <- add.constraint(
  portfolio = pspec.sd,
  type = "group",
  groups = list(
    c(1, 2, 3),
    c(4, 5, 6)
  ),
  group_min = c(0, 0.1),
  group_max = c(0.5, 0.6)
)

# objectives: mean as return, ES as risk
pspec.sd <- add.objective(pspec.sd, type = "return", name = "mean")
pspec.sd <- add.objective(pspec.sd, type = "risk", name = "StdDev")

# linear programming solvers
QP.solvers <- c("osqp", "mco", "GenSA", "DEoptim", "pso", "random")

# parallel solution
registerDoParallel(8)

QP.results <- foreach(
  i = QP.solvers, .combine = "rbind",
  .packages = c(
    "PortfolioAnalytics",
    "osqp", "Rglpk", "mco"
  )
) %dopar% {
  rtime <- system.time(
    weight <- optimize.portfolio(
      R = returns, portfolio = pspec.sd,
      optimize_method = i, trace = FALSE,
      message = FALSE
    )$weights
  )

  portfolio <- returns %*% weight
  p.sd <- sd(portfolio)
  p.mean <- mean(portfolio)
  r <- c(
    p.mean, p.sd, p.mean / p.sd,
    rtime["elapsed"], weight
  )
  r
}
stopImplicitCluster()

# results style
rownames(QP.results) <- QP.solvers
colnames(QP.results) <- c(
  "mean", "sd", "Sharpe",
  "time", colnames(returns)
)

