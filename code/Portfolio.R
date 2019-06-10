library(PortfolioAnalytics)
library(quadprog)
library(osqp)

rm(list = ls())

source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")

# Data
result <- read.csv("~/GitHub/GSoC/data/.combined.csv")
result <- xts(result[,2:13], order.by = as.Date(as.character(result[,1]), format = "%m/%d/%Y"))

GSoC.CTA <- portfolio.spec(assets = colnames(result))

GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", min_sum = 0, max_sum = 1)
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
group_list <- list(group1=c(1, 3, 5),
                   group2=c(2, 4),
                   groupA=c(2, 4, 5),
                   groupB=c(1, 3))
GSoC.CTA <- add.constraint(portfolio=GSoC.CTA, type="group",
                        groups=group_list,
                        group_min=c(0.15, 0.25, 0.2, 0.1),
                        group_max=c(0.65, 0.55, 0.5, 0.4))

# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "return", return_target = 0.07)

GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")

methodsList <- c("DEoptim", "random", "pso", "GenSA", "osqp")

test <- function(method) {
  start <- Sys.time()
  final <- optimize.portfolio(R = result, GSoC.CTA, optimize_method = method, verbos = 0)
  w <- final$weights
  print(paste0("Method: ", method))
  print(w)
  print(paste0("Sharpe Ratio: ", round((mean(result %*% w) / sd(result %*% w)), 2)))
  print(round((Sys.time() - start),2))
}

test("osqp")

for (i in methodsList) {
  test(i)
}