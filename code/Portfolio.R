install.packages('~/GitHub/PortfolioAnalytics, repos = NULL, type = "source')
library(PortfolioAnalytics)
library(quadprog)
library(osqp)

# Data
result <- read.csv("~/GitHub/GSoC/data/.combined.csv")
result <- xts(result[,2:13], order.by = as.Date(as.character(result[,1]), format = "%m/%d/%Y"))

source("~/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")

CTAs <- colnames(result)
GSoC.CTA <- portfolio.spec(assets = CTAs)

GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", min_sum = 1, max_sum = 1)
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "return", return_target = 0.007)
# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "position_limit", max_pos = 3)
# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "diversification", div_target = 0.7)
# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "turnover", turnover_target = 0.2)

GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "VaR")


final <- optimize.portfolio(R = result, GSoC.CTA, optimize_method = "osqp", verbos = 0)


