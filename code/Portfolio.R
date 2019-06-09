library(PortfolioAnalytics)
library(quadprog)
library(osqp)

source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")

# Data
result <- read.csv("~/GitHub/GSoC/data/.combined.csv")
result <- xts(result[,2:13], order.by = as.Date(as.character(result[,1]), format = "%m/%d/%Y"))

GSoC.CTA <- portfolio.spec(assets = colnames(result))

GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", min_sum = 0.5, max_sum = 1.5)
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "return", return_target = 0.007)

GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")

methodsList <- c("DEoptim", "random", "pso", "GenSA", "osqp")

test <- function(method) {
  final <- optimize.portfolio(R = result, GSoC.CTA, optimize_method = method, verbos = 0)
  w <- final$weights
  print(method)
  print(sum(w))
  print(mean(result %*% w) / sd(result %*% w))
}

for (i in methodsList) {
  test(i)
}