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
GSoC <- colnames(combinedData)
pspec <- portfolio.spec(assets = GSoC)
# make a no leverage, long only portfolio based on given 12 CTAs
pspec <- add.constraint(portfolio=pspec, type="full_investment")
pspec <- add.constraint(portfolio=pspec, type="long_only")
pspec <- add.constraint(portfolio = pspec, type="position_limit", max_pos=10)
# we want to maximine return per sd
pspec <- add.objective(pspec, type="return", name="mean")
pspec <- add.objective(pspec, type="risk", name="StdDev")

portfolioDetail <- optimize.portfolio.rebalancing(R=combinedData, pspec,rebalance_on='months',
                                                 training_period = 12)

weights <- extractWeights(portfolioDetail)
returns <- Return.rebalancing(R=combinedData, weights=extractWeights(portfolioDetail))


