
library(osqp)
why <- function(n){
  data <- read.csv(file = "aaa.csv")[,2:13]

  P <- as.matrix(data[1:12,], 12)
  q <- data[13,]
  A <- as.matrix(data[14:(13+n),], n)
  u <- c(as.numeric(data[29,]),1,1)[1:n]
  l <- c(as.numeric(data[28,]),0,0)[1:n]
  
  solve_osqp(P,q,A,l,u,pars = osqpSettings(verbose = 0))$x
}

for (i in 1:14) {
  print(round(why(i),2))
}