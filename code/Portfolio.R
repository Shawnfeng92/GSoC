library(PortfolioAnalytics)
library(quadprog)
library(osqp)

rm(list = ls())

source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")

# Data
result <- read.csv("~/GitHub/GSoC/data/.combined.csv")
result <- xts(result[,2:13], order.by = as.Date(as.character(result[,1]), format = "%m/%d/%Y"))

GSoC.CTA <- portfolio.spec(assets = colnames(result))

GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", min_sum = -1, max_sum = 1)
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
pspec <- add.constraint(portfolio=pspec,
                        type="group",
                        groups=list(c(1, 2, 1), 4),
                        group_min=c(0.1, 0.15),
                        group_max=c(0.85, 0.55),
                        group_labels=c("GroupA", "GroupB"),
                        group_pos=c(2, 1))
# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "return", return_target = 0.07)

GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")

methodsList <- c("DEoptim", "random", "pso", "GenSA", "osqp")

test <- function(method) {
  start <- Sys.time()
  final <- optimize.portfolio(R = result, GSoC.CTA, optimize_method = method, verbos = 0)
  w <- final$weights
  print(paste0("Method: ", method))
  print(paste0("Sharpe Ratio: ", round((mean(result %*% w) / sd(result %*% w)), 2)))
  print(round((Sys.time() - start),2))
}

# test("osqp")

for (i in methodsList) {
  test(i)
}