{ 
  library(PortfolioAnalytics)
  library(quadprog)
  library(osqp)
  library(Rglpk)
  library(DEoptim)
  library(foreach)
  library(doSNOW)
  library(mco)
  library(data.table)
  library(GenSA)
  library(pso)
  library(Quandl)
}

rm(list = ls())

source("~/GitHub/GSoC/Final Evaluation/optimize.portfolio.R")
data <- read.csv("~/GitHub/GSoC/data/Returns/.combined.csv")
returns <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1]), format = "%m/%d/%Y"))


ES <- function(x) {
  return(mean(x[which(x<quantile(x,0.05))]))
}

# comparison test on Rglpk
# linear programming problems with mean return reward and ES risk

pspec <- portfolio.spec(assets=colnames(returns))

pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "ES")

pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1.1)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
group_list <- list(group1=c(1, 3, 5),
                   group2=c(2, 4),
                   groupA=c(2, 4, 5),
                   groupB=c(1, 3))
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=group_list,
                        group_min=c(0.15, 0.25, 0.2, 0.1),
                        group_max=c(0.65, 0.55, 0.5, 0.4))
lRglpk <- optimize.portfolio(returns, pspec, optimize_method = "Rglpk", sil = 1)
lGenSA <- optimize.portfolio(returns, pspec, optimize_method = "GenSA")
lpso <- optimize.portfolio(returns, pspec, optimize_method = "pso")
lDEoptim <- optimize.portfolio(returns, pspec, optimize_method = "DEoptim")
lrandom <- optimize.portfolio(returns, pspec, optimize_method = "random")

r1Rglpk <- returns %*% lRglpk$weights
r1GenSA <- returns %*% lGenSA$weights
r1pso <- returns %*% lpso$weights
r1DEoptim <- returns %*% lDEoptim$weights
r1random <- returns %*% lrandom$weights

meanoveres <- c(mean(r1Rglpk)/ES(r1Rglpk),
                mean(r1GenSA)/ES(r1GenSA),
                mean(r1pso)/ES(r1pso),
                mean(r1DEoptim)/ES(r1DEoptim),
                mean(r1random)/ES(r1random))

runningtimes <- c(lRglpk$elapsed_time,
                  lGenSA$elapsed_time,
                  lpso$elapsed_time,
                  lDEoptim$elapsed_time,
                  lrandom$elapsed_time)
result1 <- rbind(c("Rglpk", "GenSA", "pso", "DEoptim", "random"),
                meanoveres, 
                runningtimes)

# comparison test on osqp
# quadratic programming problems with mean return reward and volatility risk

pspec <- portfolio.spec(assets=colnames(returns))

pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "StdDev")

pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1.1)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
group_list <- list(group1=c(1, 3, 5),
                   group2=c(2, 4),
                   groupA=c(2, 4, 5),
                   groupB=c(1, 3))
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=group_list,
                        group_min=c(0.15, 0.25, 0.2, 0.1),
                        group_max=c(0.65, 0.55, 0.5, 0.4))

qosqp <- optimize.portfolio(returns, pspec, optimize_method = "osqp")
qGenSA <- optimize.portfolio(returns, pspec, optimize_method = "GenSA")
qpso <- optimize.portfolio(returns, pspec, optimize_method = "pso")
qDEoptim <- optimize.portfolio(returns, pspec, optimize_method = "DEoptim")
qrandom <- optimize.portfolio(returns, pspec, optimize_method = "random")

r2osqp <- returns %*% qosqp$weights
r2GenSA <- returns %*% qGenSA$weights
r2pso <- returns %*% qpso$weights
r2DEoptim <- returns %*% qDEoptim$weights
r2random <- returns %*% qrandom$weights

meanoversigma <- c(mean(r2osqp)/sd(r2osqp),
                mean(r2GenSA)/sd(r2GenSA),
                mean(r2pso)/sd(r2pso),
                mean(r2DEoptim)/sd(r2DEoptim),
                mean(r2random)/sd(r2random))

runningtimes <- c(qosqp$elapsed_time,
                  qGenSA$elapsed_time,
                  qpso$elapsed_time,
                  qDEoptim$elapsed_time,
                  qrandom$elapsed_time)
result2 <- rbind(c("osqp", "GenSA", "pso", "DEoptim", "random"),
                 meanoversigma, 
                runningtimes)

# comparison test on mco
# general convex optimize problems with mean return reward and volatility risk

pspec <- portfolio.spec(assets=colnames(returns))

pspec <- add.objective(pspec, type = "return", name = "mean")
pspec <- add.objective(pspec, type = "risk", name = "StdDev")

pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1.1)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
group_list <- list(group1=c(1, 3, 5),
                   group2=c(2, 4),
                   groupA=c(2, 4, 5),
                   groupB=c(1, 3))
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=group_list,
                        group_min=c(0.15, 0.25, 0.2, 0.1),
                        group_max=c(0.65, 0.55, 0.5, 0.4))
pspec <- add.constraint(portfolio = pspec, type = "position_limit", max_pos = 6)
pmco <- optimize.portfolio(returns, pspec, optimize_method = "mco", sil = 1)
pGenSA <- optimize.portfolio(returns, pspec, optimize_method = "GenSA")
ppso <- optimize.portfolio(returns, pspec, optimize_method = "pso")
pDEoptim <- optimize.portfolio(returns, pspec, optimize_method = "DEoptim")
prandom <- optimize.portfolio(returns, pspec, optimize_method = "random")

r3mco <- returns %*% pRglpk$weights
r3GenSA <- returns %*% pGenSA$weights
r3pso <- returns %*% ppso$weights
r3DEoptim <- returns %*% pDEoptim$weights
r3random <- returns %*% prandom$weights

meanoveres <- c(mean(r3mco)/ES(r3mco),
                mean(r3GenSA)/ES(r3GenSA),
                mean(r3pso)/ES(r3pso),
                mean(r3DEoptim)/ES(r3DEoptim),
                mean(r3random)/ES(r3random))

runningtimes <- c(pRglpk$elapsed_time,
                  pGenSA$elapsed_time,
                  ppso$elapsed_time,
                  pDEoptim$elapsed_time,
                  prandom$elapsed_time)
result3 <- rbind(c("Rglpk", "GenSA", "pso", "DEoptim", "random"),
                meanoveres, 
                runningtimes)







