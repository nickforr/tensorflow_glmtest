
# Introduction ------
# Examples below set out how we might re-design the go algorithm to allow for
# matrix multiplication via gpus

# Libraries used ------
# Note tensorflow won't use gpus unless specifically installed to do so (which
# is unlikely to be the case on a laptop (see
# https://tensorflow.rstudio.com/tensorflow/articles/installation_gpu.html).
# This has been included so that I can test this on a paperspace gpu machine
# (see www.paperspace.com)

library(microbenchmark) # to benchmark times
library(purrr) # for R-based loops via map & accumulate
library(gpuR) # to access gpu on laptop
library(tensorflow) # for testing on gpu in paperspace

# Simulation constants ------
set.seed(1.32)
nsim <- 5000 # let's go with 5000 sims...
nproj <- 70 * 12 + 1 # monthly timesteps for 70 yrs (+1 is for time 0)

# Create simulated data ------
# Use rows for time
# As a future step to test, might want to explore alternative rebalancing periods

# Set up returns for 2 funds (one for existing assets and one for new cbns)
rtnsA <- matrix(rnorm(nsim * nproj, mean = 0.05, sd = 0.10), nrow = nproj)
rtnsA[1, ] <- 0 # Ensure first row is zero
rtnsB <- matrix(rnorm(nsim * nproj, mean = 0.07, sd = 0.14), nrow = nproj)
rtnsB[1, ] <- 0 # Ensure first row is zero

# Will also transform these to indices outside of any core calcs
rtnIndicesA <- apply(1 + rtnsA, 2, cumprod)
rtnIndicesB <- apply(1 + rtnsB, 2, cumprod)

# Set up salary increase returns
salRtns <- matrix(rnorm(nsim * nproj, mean = 0.02, sd = 0.01), nrow = nproj)
salRtns[1, ] <- 0
salIndex <- apply(1 + salRtns, 2, cumprod)


# Member details ------
initialPot <- 100000
initialSalary <- 30000




