library(PortfolioAnalytics)
source("~/GitHub/GSoC/optimize.portfolio.R")

data(edhec)
returns <- edhec[,1:4]
fund.names <- colnames(returns)
pspec <- portfolio.spec(assets = fund.names)

# Add the full investment constraint that specifies the weights must sum to 1. 
# pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1) 
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=-1, max_sum=1) 

# The full investment constraint can also be specified with type="full_investment" 
pspec <- add.constraint(portfolio=pspec, type="full_investment")

# Another common constraint is that portfolio weights sum to 0. 
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0, max_sum=0)
pspec <- add.constraint(portfolio=pspec, type="dollar_neutral")
pspec <- add.constraint(portfolio=pspec, type="active")

# Add box constraints 
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.4)

# min and max can also be specified per asset 
pspec <- add.constraint(portfolio=pspec,
                        type="box",
                        min=c(0.05, 0, 0.08, 0.1),
                        max=c(0.4, 0.3, 0.7, 0.55))

# A special case of box constraints is long only where min=0 and max=1 
# The default action is long only if min and max are not specified 
pspec <- add.constraint(portfolio=pspec, type="box")
pspec <- add.constraint(portfolio=pspec, type="long_only")

# Add group constraints 
pspec <- add.constraint(portfolio=pspec,
                        type="group",
                        groups=list(c(1, 2, 1), 4),
                        group_min=c(0.1, 0.15),
                        group_max=c(0.85, 0.55),
                        group_labels=c("GroupA", "GroupB"),
                        group_pos=c(2, 1))

# Add position limit constraint such that we have a maximum number 
# of three assets with non-zero weights. 
pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)

# Add diversification constraint 
pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)

# Add turnover constraint 
pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.2)

# Add target mean return constraint 
pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.007)

# Creates a new portfolio object using pspec and adds a quadratic utility 
# objective. This will add two objectives to the portfolio object; 1) mean and 
# 2) var. The risk aversion parameter is commonly referred to as lambda in the 
# quadratic utility formulation that controls how much the portfolio variance 
# is penalized. 
pspec.maxQU <- add.objective(pspec, type="quadratic_utility", risk_aversion=0.25) 

# Creates a new portfolio object using pspec and adds mean as an objective 
pspec.maxMean <- add.objective(pspec, type="return", name="mean") 

# Creates a new portfolio object using pspec and adds StdDev as an objective 
pspec.minStdDev <- add.objective(pspec, type="risk", name="StdDev") 

# Creates a new portfolio object using pspec and adds ES as an objective. 
# Note that arguments to ES are passed in as a named list. 
pspec.minES <- add.objective(pspec, 
                             type="risk", 
                             name="ES", 
                             arguments=list(p=0.925, clean="boudt")) 

# Creates a new portfolio object using pspec.minES and adds a risk budget 
# objective with limits on component risk contribution. 
# Note that arguments to ES are passed in as a named list. 
pspec.RiskBudgetES <- add.objective(pspec.minES, 
                                    type="risk_budget", 
                                    name="ES", 
                                    arguments=list(p=0.925, clean="boudt"), 
                                    min_prisk=0, max_prisk=0.6) 

# Creates a new portfolio object using pspec.minES and adds a risk budget 
# objective with equal component risk contribution. 
# Note that arguments to ES are passed in as a named list. 
pspec.EqRiskES <- add.objective(pspec.minES, 
                                type="risk_budget", 
                                name="ES", 
                                arguments=list(p=0.925, clean="boudt"), 
                                min_concentration=TRUE) 

# Creates a new portfolio object using pspec and adds a weight_concentration 
# objective. The conc_aversion parameter controls how much concentration is 
# penalized. The portfolio concentration is defined as the Herfindahl Hirschman 
# Index of the weights. 
pspec.conc <- add.objective(pspec, type="weight_concentration", name="HHI", conc_aversion=0.01)

optimize.portfolio(R = returns, portfolio = pspec.maxMean, optimize_method = "pso")
