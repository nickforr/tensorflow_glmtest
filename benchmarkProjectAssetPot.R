
library(purrr)
library(microbenchmark)
library(Rcpp)
source("projectAssetPot.R")

rtns <- # Monthly
  matrix(
    rnorm(10000 * 12 * 100, mean = 0.04 / 12, sd = 0.1 / sqrt(12)), 
    nrow = 12 * 100)

initialPot <- 100
flows <- matrix(-5 / 12, nrow = 1, ncol = 10000) # monthly
flowTiming <- 0.5
adjustments <- matrix(3 / 12, nrow = 1, ncol = 10000) # monthly
adjustmentsTiming <- 1
outputAssetOnly = FALSE

# R project
microbenchmark({
  rOutput <- 
    projectAssetPot(
      rtns = rtns, initialPot = initialPot, flows = flows, 
      flowTiming = flowTiming, adjustments = adjustments, 
      adjustmentsTiming = adjustmentsTiming, outputAssetOnly = TRUE, 
      useCpp = FALSE)
}, times = 100, unit = "ms")

# Rcpp project
microbenchmark({
  cppOutput <- 
    projectAssetPot(
      rtns = rtns, initialPot = initialPot, flows = flows, 
      flowTiming = flowTiming, adjustments = adjustments, 
      adjustmentsTiming = adjustmentsTiming, outputAssetOnly = TRUE, 
      useCpp = TRUE)
}, times = 100, unit = "ms")

# Check output equal
all.equal(rOutput[["assets"]], cppOutput[["assets"]])
all.equal(rOutput[["flowsPaid"]], cppOutput[["flowsPaid"]])





