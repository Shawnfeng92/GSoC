library(SWIM)
library(zoo)
library(xts)
library(mvtnorm)

data <- read.csv("~/GitHub/GSoC/data/Returns/.combined.csv")

data <- xts(data[,2:13], as.Date(data[,1], format = "%m/%d/%Y"))

model <- stress(type = "mean", data, 1:12, rep(0,12))

summary(model)

set.seed(0)
x <- data.frame(cbind(
  "normal" = rnorm(1000),
  "gamma" = rgamma(1000, shape = 2),
  "beta" = rbeta(1000, shape1 = 2, shape2 = 2)))
## stressing mean and sd of column 1
res1 <- stress(type = "mean sd", x = x, k = 1, new_means = 0.1,
               new_sd = 1.1, method = "Newton",
               control = list(maxit = 1000, ftol = 1E-15))
summary(res1)
## calling stress_mean_sd directly
res2 <- stress_mean_sd(x = x, k = 1, new_means = 0.1,
                       new_sd = 1.1, method = "Newton",
                       control = list(maxit = 1000, ftol = 1E-15))
