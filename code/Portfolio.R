# Depended Packages:----
library(PortfolioAnalytics)
library(zoo)
library(xts)
library(rootSolve)
library(foreach)
library(PerformanceAnalytics)
# Suggested Packages:----
library(quantmod)
library(DEoptim)
library(iterators)
library(fGarch)
library(Rglpk)
library(quadprog)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI.plugin.symphony)
library(pso)
library(GenSA)
library(corpcor)
library(testthat)
library(nloptr)
library(MASS)
library(robustbase)
# Data----
result <- read.csv(".combined.csv")
result1 <- result[,2:13]
imputed_Data <- mice(result1, m=1, maxit = 50, method = 'pmm', seed = 500)
result[,2:13] <- complete(imputed_Data)
combinedData <- result
combinedData[,1] <- as.Date(as.character(combinedData[,1]), format='%m/%d/%Y')
combinedData <- as.xts(combinedData[,2:13],combinedData[,1])
rm(result, result1, imputed_Data)
# Portfolio ----
CTAs <- colnames(combinedData)
GSoC <- portfolio.spec(assets = CTAs)
# make a no leverage, long only portfolio based on given 12 CTAs
GSoC <- add.constraint(portfolio = GSoC, type = "full_investment")
GSoC <- add.constraint(portfolio = GSoC, type = "long_only")
GSoC <- add.constraint(portfolio = GSoC, type = "position_limit", max_pos=10)
# we want to maximine return per sd
GSoC <- add.objective(GSoC, type = "return", name = "mean")
GSoC <- add.objective(GSoC, type = "risk", name = "StdDev")

portfolioDetail <- optimize.portfolio.rebalancing(R = combinedData, GSoC,rebalance_on='months',
                                                 training_period = 12)

weights <- extractWeights(portfolioDetail)
returns <- Return.rebalancing(R=combinedData, weights=extractWeights(portfolioDetail))


