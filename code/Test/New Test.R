library(PortfolioAnalytics)
library(osqp)
library(Rglpk)
library(mco)

source(file = "code/Test/optimize.portfolio.R")

data(edhec)
returns <- edhec
fund.names <- colnames(returns)

pspec <- portfolio.spec(assets = fund.names)

# Add the full investment constraint that specifies the weights must sum to 1.
pspec <- add.constraint(
  portfolio = pspec, type = "weight_sum",
  min_sum = 1, max_sum = 1
)

# min and max can also be specified per asset
pspec <- add.constraint(
  portfolio = pspec,
  type = "box",
  min = c(0.05, 0, 0.08, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  max = c(0.4, 0.3, 0.7, 0.55, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

# Add group constraints
pspec <- add.constraint(
  portfolio = pspec,
  type = "group",
  groups = list(c(1, 2), 4),
  group_min = c(0.1, 0.15),
  group_max = c(0.85, 0.55),
  group_labels = c("GroupA", "GroupB")
)

# Add position limit constraint such that we have a maximum number
# of three assets with non-zero weights.
# pspec <- add.constraint(portfolio = pspec, type = "position_limit", max_pos = 3)

# Add diversification constraint
pspec <- add.constraint(portfolio = pspec, type = "diversification", div_target = 0.7)

# Add target mean return constraint
pspec <- add.constraint(portfolio = pspec, type = "return", return_target = 0.007)

# Add objective
pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "StdDev")

result <- optimize.portfolio(R = returns, portfolio = pspec, optimize_method = "pso",
                             trace = FALSE, message = FALSE)
