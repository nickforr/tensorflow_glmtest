

library(microbenchmark)
library(purrr)
#library(tensorflow)

nsim <- 5000
nproj <- 71 * 12

set.seed(1.23)

# set up a list of weights to base analysis on
weights <-
  list(
    ukEquity = 0.1,
    osEquity = 0.1,
    emEquity = 0.1,
    privateEquity = 0.1,
    property = 0.1,
    cash = 0.1,
    medIlg = 0.1,
    medFig = 0.1,
    medCorp = 0.1,
    highYieldDebt = 0.1
  )

# set up general random matrices
rtnList <-
  purrr::map(weights,
             ~matrix(
               rnorm(
                 n = nproj * nsim,
                 mean = runif(1, 0, 5) / 100,
                 sd = runif(1, 10, 20) / 100),
               nrow = nproj, ncol = nsim)
  )

# bind by rows to form large matrix for gpu use
rtnMatrix <- do.call(rbind, rtnList)

# need a function to create appropriate weights matrix
createWtMatrix <- function(wtsList, nrows) {
  do.call(cbind, purrr::map(wtsList, ~diag(.x, nrow = nrows)))
}

# create random wts
createRndWts <- function(pfolioNames, assetNames, nproj) {
  purrr::map(pfolioNames, ~purrr::map(assetNames, ~runif(1)))
}

pfolios <- letters[1:10]
rndWts <- createRndWts(pfolios, names(weights), nproj)
rndWtsMatrix <- 
  do.call(rbind, purrr::map(rndWts, createWtMatrix, nrows = nproj))

# R only approach using matrices
microbenchmark({
  rMatrixAllRtns <- rndWtsMatrix %*% rtnMatrix
}, times = 1)

# gpu approach using tensorflow
# sess <- tf$Session()
# microbenchmark({
#   tfRtns <- tf$constant(rtnMatrix)
#   tfRndWts <- tf$constant(rndWtsMatrix)
#   tfMatrixAllRtns <- tf$matmul(tfRndWts, tfRtns)
#   tfResult = sess$run(tfMatrixAllRtns)
# }, times = 1)
# sess$close()
