# Easy Test ----
#
# In this part, I have already instralled all package locally, so I
# can call them and use them by function library()
#

# Depended Packages
library(PortfolioAnalytics)
library(zoo)
library(xts)
library(rootSolve)
library(foreach)
library(PerformanceAnalytics)
# Suggested Packages
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
library(mice)
library(ggplot2)

# Intermediate Test----
# 
# In this test, I will use PortfolioAnalytics to make two quarterly rebalanced
# pofolio based on ETF and CTA universal. Then observe their monthly return and
# compare to return of S&P 500.
#
 
# Data
result <- read.csv("C:/Users/Shawn/Documents/GitHub/GSoC/data/.combined.csv")
result1 <- result[,2:13]
imputed_Data <- mice(result1, m=1, maxit = 50, method = 'pmm', seed = 500)
result[,2:13] <- complete(imputed_Data)
combinedData <- result
combinedData[,1] <- as.Date(as.character(combinedData[,1]), format='%m/%d/%Y')
combinedData <- as.xts(combinedData[,2:13],combinedData[,1])
rm(result, result1, imputed_Data)

# CTA Portfolio 
CTAs <- colnames(combinedData)
GSoC.CTA <- portfolio.spec(assets = CTAs)

# make a no leverage, long only portfolio based on given 12 CTAs
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "full_investment")
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "position_limit", max_pos = 10)

# we want to maximine return per sd
GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")
portfolioDetail.CTA <- optimize.portfolio.rebalancing(R = combinedData, GSoC.CTA, rebalance_on = 'quarters',
                                                      optimize_method = "DEoptim", training_period = 12)
return.CTA <- Return.rebalancing(R = combinedData, weights=extractWeights(portfolioDetail.CTA))

# ETF Portfolio
combinedData <- read.csv("C:/Users/Shawn/Documents/GitHub/GSoC/data/..return.sample.ETF.monthly.csv")
combinedData[,1] <- as.Date(as.character(combinedData[,1]), format='%m/%d/%Y')
combinedData <- as.xts(combinedData[,-1],combinedData[,1])
ETFs <- colnames(combinedData)
GSoC.ETF <- portfolio.spec(assets = ETFs)

# make a no leverage, long only portfolio based on given ETF universal
GSoC.ETF <- add.constraint(portfolio = GSoC.ETF, type = "full_investment")
GSoC.ETF <- add.constraint(portfolio = GSoC.ETF, type = "long_only")
GSoC.ETF <- add.constraint(portfolio = GSoC.ETF, type = "position_limit", max_pos = 10)
GSoC.ETF <- add.constraint(portfolio = GSoC.ETF, type="box", min = 0, max = 0.3)

# we want to maximine return per sd
GSoC.ETF <- add.objective(GSoC.ETF, type = "return", name = "mean")
GSoC.ETF <- add.objective(GSoC.ETF, type = "risk", name = "StdDev")
portfolioDetail.ETF <- optimize.portfolio.rebalancing(R = combinedData, GSoC.ETF,optimize_method = "DEoptim", 
                                                      rebalance_on = 'quarters', training_period = 12)
return.ETF <- Return.rebalancing(R = combinedData, weights=extractWeights(portfolioDetail.ETF))

# Comparison to S&P 500 
SP <- read.csv("C:/Users/Shawn/Documents/GitHub/GSoC/data/..return.gspc.monthly.csv")
SP[,1] <- as.Date(as.character(SP[,1]), format='%m/%d/%Y')
SP <- as.xts(SP[,2],SP[,1])
comparison <- c()
comparison <- cbind(SP["2015/20190201"],return.ETF,return.CTA)
colnames(comparison) <- c("S&P", "ETF Portfolio", "CTA Portfolio")

colors <- rainbow(3)
png(file="C:/Users/Shawn/Documents/GitHub/GSoC/result/MonthlyReturn.png", width = 1000, height = 500, units = "px")
plot(comparison, ylim = c(-0.2,0.2), col = colors, main = "Monthly Return")
addLegend("topright", colnames(comparison), lty = 1, col=colors)
dev.off()

png(file="C:/Users/Shawn/Documents/GitHub/GSoC/result/CumulativeReturn.png", width = 1000, height = 500, units = "px")
plot(cumsum(comparison), col = colors, main = "Cumulative Return")
addLegend("topleft", colnames(comparison), lty = 1, col=colors)
dev.off()

# Hard Test ----
#
# In this test, I will use solver in quadprog to creat a optimal portfolio
# based on ETF data from 2013-11-01 to 2019-02-01. If it works, we can easily
# do a parameter transform and implement it to PortfolioAnalytics.
#

# First, if we allow short ETF:
nAsset <- ncol(combinedData)

solQP <- solve.QP(Dmat = cov(combinedData), 
                  dvec = rep(0, nAsset),
                  Amat = as.matrix(apply(combinedData, 2, mean)),
                  bvec = 1,
                  meq = 0)

w <- as.matrix(solQP$solution/sum(solQP$solution))
print(round(w,4))


# Then, let's forbiden ETF short selling:
AmatNS <- cbind(as.matrix(apply(combinedData, 2, mean)),diag(1,nAsset))
bvecNS <- c(1,rep(0,nAsset))

solQP <- solve.QP(Dmat = cov(combinedData), 
                  dvec = rep(0, nAsset),
                  Amat = AmatNS,
                  bvec = bvecNS,
                  meq = 0)

w <- as.matrix(solQP$solution/sum(solQP$solution))
print(round(w,4))


