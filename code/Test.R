library(PortfolioAnalytics)
library(quadprog)
library(osqp)
library(Rglpk)
library(foreach)

rm(list = ls())

source("~/Documents/GitHub/PortfolioAnalytics/R/optimize.portfolio.R")

# Data
data <- read.csv("~/Documents/GitHub/GSoC/data/fake.csv")
data <- xts(data[,2:ncol(data)], order.by = as.Date(as.character(data[,1]), format = "%Y-%m-%d"))

GSoC.CTA <- portfolio.spec(assets = colnames(data))

GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", min_sum = -1, max_sum = 1)
GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
# group_list <- list(group1=c(1, 3, 5),
#                    group2=c(2, 4),
#                    groupA=c(2, 4, 5),
#                    groupB=c(1, 3))
# GSoC.CTA <- add.constraint(portfolio=GSoC.CTA, type="group",
#                         groups=group_list,
#                         group_min=c(0.15, 0.25, 0.2, 0.1),
#                         group_max=c(0.65, 0.55, 0.5, 0.4))

# GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "return", return_target = 0.07)

GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")

methodsList <- c("DEoptim", "random", "pso", "GenSA", "osqp")

test <- function(x) {
  result <- optimize.portfolio(R = data, GSoC.CTA, optimize_method = "Rglpk", verbos = 0, alpha = 0.05)
  result <- data %*% result$weights
  return(mean(result[which(result < quantile(result, 0.05))]))
}

optimize.portfolio(R = data, GSoC.CTA, optimize_method = "osqp")
cl <- makeCluster(8)
registerDoParallel(cl)
result <- foreach (i = 1:(ncol(data)/10), .combine = "rbind", .packages = c("PortfolioAnalytics", "osqp")) %dopar% {
  GSoC.CTA <- portfolio.spec(assets = colnames(data[,1:(10*i)]))
  GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "weight_sum", min_sum = -1, max_sum = 1)
  GSoC.CTA <- add.constraint(portfolio = GSoC.CTA, type = "long_only")
  GSoC.CTA <- add.objective(GSoC.CTA, type = "return", name = "mean")
  GSoC.CTA <- add.objective(GSoC.CTA, type = "risk", name = "StdDev")
  
  time <-system.time(
    optimize.portfolio(R = data[,1:(10*i)], GSoC.CTA, optimize_method = "osqp")
  )
  rm("GSoC.CTA")
  temp <- c(10*i, time[3]) 
}
stopCluster(cl)


# -----
# 
# CVaR <- c()
# return <- seq(from = -0.01, to = 0.01, length.out = 10)
# 
# for (i in return) {
#   CVaR <- c(CVaR, test(i))
# }
# plot( - CVaR, return, "l")

# result <- c()
# 
# for (i in methodsList) {
#   start <- Sys.time()
#   w <- test(i)
#   w[which(w<0)] <- 0
#   result <- rbind(result, c(mean(data %*% w), 
#                             sd(data %*% w), mean(data %*% w)/sd(data %*% w),
#                             as.numeric(Sys.time() - start, units = "secs"),w))
# }
# 
# colnames(result) <- c("mean", "sigma", "sharpe ratio", "running time",colnames(data))
# rownames(result) <- methodsList
# 
# result <- round(result*100,2)
# result[,4] <- result[,4]/100 
# 
# result <- c()
# for (i in 1:100) {
#   w <- test("osqp")
#   result <- c(result, mean(data %*% w)/sd(data %*% w))
# }
