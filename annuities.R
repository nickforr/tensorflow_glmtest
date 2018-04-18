
# Introduction ------

# Examples below set out how we might re-implement the annuity price algorithm
# using Rcpp to speed up the loops.

# Libraries used ------
library(microbenchmark) # to benchmark times
library(Rcpp) # integrating C++ and R
library(RcppArmadillo) # using C++ Armadillo linear algebra library

# Simulation constants ------
set.seed(1.32)
nsim <- 5000 # let's go with 5000 sims...
nyrs <- 71

# Create simulated data ------
# Use rows for time
yieldData <- matrix(rnorm(nyrs * nsim, mean = 0.03, sd = 0.02), nrow = nyrs)
paymentProbs <- rev(seq_len(nyrs)) / nyrs

# R approach (refactored version from mortr)
priceAnnuity_R <- function(yieldData, paymentProbs) {
  
  annuityPrices <- yieldData * 0 #use yieldData to get 0 matrix of right dims
  
  for (i in seq_len(nrow(annuityPrices))) {
    
    combinedProbs <- #payment probs based on projection time
      if (i == 1) {
        paymentProbs
      } else {
        utils::tail(paymentProbs, -i + 1) / paymentProbs[i]
      }
    
    yieldSims <- yieldData[i, ]
    logYields <- log(1 + yieldSims)
    maturities <- -(seq_along(combinedProbs) - 1)
    projYields <- exp(tcrossprod(logYields, maturities))
    price <- projYields %*% combinedProbs
    annuityPrices[i, ] <- price
  }
  annuityPrices
}
microbenchmark::microbenchmark({
  annuityPrices_R <- 
    priceAnnuity_R(yieldData = yieldData, paymentProbs = paymentProbs)
}, times = 50)


Rcpp::sourceCpp("cppAnnuity.cpp")
microbenchmark::microbenchmark({
  annuityPrices_cpp <- priceAnnuityCpp(yieldData, paymentProbs)
}, times = 50)
# Check this equals initial R implementation
all.equal(annuityPrices_R, annuityPrices_cpp)

Rcpp::sourceCpp("cppArmaAnnuity.cpp")
microbenchmark::microbenchmark({
  annuityPrices_cppArma <- armaPriceAnnuity(yieldData, paymentProbs)
}, times = 50)
# Check this equals initial R implementation
all.equal(annuityPrices_R, annuityPrices_cppArma)

# Conclusions ------

# Based on initial tests, I was getting xxx seconds for 5000 sims and 70
# years of projections under the R implementation.

# Making use of Rcpp more than yyy the time of the R approach to
# c. zzz milliseconds.

