

args <- commandArgs(trailingOnly = TRUE)
# Purpose is to illustrate how we might value cashflows via matrix
# multiplication, building off the format of how we would

set.seed(1.23)
nsim <- if (length(args) == 0) 5000 else as.numeric(args[1])
nproj <- 31
maxMaturity <- 150
ncflowGroups <- if (length(args) <= 1) nproj * 30 else as.numeric(args[2]) #nproj * 3

cputypeforsavename <- if (length(args) <= 2) "unknown" else args[3]
tempsavename <- paste0(cputypeforsavename, "_", ncflowGroups, ".RDS")
tempsavetime <- paste0(cputypeforsavename, "_", ncflowGroups, "_timing.RDS")

discFactors <- matrix(runif(nsim * nproj * maxMaturity), ncol = maxMaturity)
allCflows <- matrix(runif(maxMaturity * ncflowGroups), nrow = maxMaturity)

microbenchmark::microbenchmark({
    pvCflows <- discFactors %*% allCflows
  }, times = 1)
pvCflows[1:10, 1:10]

