


# Purpose is to illustrate how we might value cashflows via matrix
# multiplication, building off the format of how we would

nsim <- 5000
nproj <- 31
maxMaturity <- 150
ncflowGroups <- nproj * 3

discFactors <- matrix(runif(nsim * nproj * maxMaturity), ncol = maxMaturity)
allCflows <- matrix(runif(maxMaturity * ncflowGroups), nrow = maxMaturity)

microbenchmark::microbenchmark({
  pvCflows <- discFactors %*% allCflows
}, times = 2)

# Basic test using nvblas is that a run of 3 seconds falls to 70 milliseconds under a gpu
