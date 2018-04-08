
# Introduction ------

# Examples below set out how we might re-design the go algorithm to allow for
# matrix multiplication via gpus

# Having played about with gpuR and tensorflow, easiest way to leverage gpus is
# through use of nvblas libraries which doesn't require any changes to R code
# (just to the way R is configured).

# Libraries used ------
library(microbenchmark) # to benchmark times
library(purrr) # for R-based loops via map & accumulate

# Simulation constants ------
set.seed(1.32)
nsim <- 10 #5000 # let's go with 5000 sims...
nyrs <- 70
nproj <- nyrs * 12 + 1 # monthly timesteps (+1 is for time 0)

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

# set some cbn rates (with a time dependency)
cbnRate <- c(0, rep(runif(nyrs, min = 5, max = 15) / 100, times = 12)) / 12 

# Loop style ------
# Whilst R can be slow in loops, will code this up to get a better comparison
microbenchmark({# Include all setup in the benchmark
  # First initialise matrix to capture fund (efficient for R)
  potProjection <- matrix(0, nrow = nproj, ncol = nsim)
  potProjection[1, ] <- initialPot
  for (isim in seq_len(nsim)) {
    sal <- initialSalary
    pot <- initialPot
    cbnPot <- 0
    for (iproj in (1 + seq_len(nproj - 1))) {
      cbn <- cbnRate[iproj] * sal
      cbnPot <- cbnPot * (1 + rtnsB[iproj, isim]) + cbn
      pot <- pot * (1 + rtnsA[iproj, isim])
      sal <- sal * (1 + salRtns[iproj, isim])
      potProjection[iproj, isim] <- pot + cbnPot
    }
  }
}, times = 10) # Run this 10 times

# Vectorised approach ------
microbenchmark({# Again, include all setup in the benchmark
  salary <- initialSalary * salIndex
  initParams <-
    list(
      startYear = 0,
      pot = initialPot, 
      cbnPot = 0, 
      output = 
        list(totalPot = initialPot)
    )
  stepFn <- function(stepParams) {
    startYear = stepParams$startYear
    projIndex <- startYear + 2
    startYrIndex <- startYear + 1
    startPot <- stepParams$pot
    startCbnPot <- stepParams$cbnPot
    stepRtns <- rtnsA[projIndex, , drop = FALSE]
    stepCbnRtns <- rtnsB[projIndex, , drop = FALSE]
    stepCbn <- cbnRate[projIndex] * salary[projIndex - 1, , drop = FALSE]
    endPot <- startPot * (1 + stepRtns)
    endCbnPot <- startCbnPot * (1 + stepCbnRtns) + stepCbn
    stepParams$startYear <- startYear + 1
    stepParams$pot <- endPot
    stepParams$cbnPot <- endCbnPot
    stepParams$output$totalPot <- endPot + endCbnPot
    return(stepParams)
  }
  funcall <- function(params, f) f(params)
  allSimResults <-
    purrr::accumulate(
      rep.int(list(stepFn), nproj - 1),
      funcall,
      .init = initParams
    )
  transposedOutput <- 
    purrr::transpose(purrr::map(allSimResults, "output"))
  matrixList <- purrr::map(transposedOutput, ~ drop(do.call(rbind, .x)))
  potProjection_R <- matrixList[["totalPot"]]
}, times = 10) # Again run 10 times

# nvblas gpu matrix multiplication approach ------
# Code is just plain R (with algorithm targeting matrix multiplication)
# An R build with nvblas should use gpu automatically where possible
microbenchmark({
  adjSalIndex <- initialSalary * salIndex / rtnIndicesB
  potProjection_matrix <- initialPot * rtnIndicesA
  tempRtnIndicesB <- rtnIndicesB # consider removing...
  for (iproj in (1 + seq_len(nproj - 1))) {
    cbn <- cbnRate[iproj] * adjSalIndex[iproj - 1, ]
    diagCbn <- diag(cbn)
    tempResult <- tempRtnIndicesB %*% diagCbn
    potProjection_matrix <- potProjection_matrix + tempResult
    tempRtnIndicesB[iproj - 1, ] <- 0
  }
}, times = 1) # Only doing this once, in case of issues with gpu code

