
rtns <- matrix(rnorm(71 * 5000, 0.04, 0.15), nrow = 71)
rtns[1, ] <- 0
initialPot <- 100
targetRegularFlowsOut <- matrix(10, nrow = 71, ncol = 5000)
targetRegularFlowsOut[1, ] <- 0

# R version
projectDrawdownPot <- function(initialPot = 0, rtns, 
  targetRegularFlowsOut = NULL) {
  
  nsim <- ncol(rtns)
  nproj <- nrow(rtns) - 1
  
  pot <- rep.int(initialPot, nsim)
  
  #Set initial parameters
  initParams <-
    list(
      startYear = 0,
      outputList =
        list(
          potSize = pot,
          actualRegularFlowsOut = rep(0, nsim)
        )
    )
  
  stepFn <- function(stepParams) {
    
    startYear = stepParams$startYear
    projIndex <- startYear + 2
    startYrIndex <- startYear + 1
    
    startAssets <- stepParams$outputList$potSize
    
    #Regular withdrawal amounts next (at start of year)
    actualRegularFlowsOut <-
      pmin(startAssets, targetRegularFlowsOut[projIndex, , drop = FALSE])
    assetsPostRegFlow <- pmax(0, startAssets - actualRegularFlowsOut)
    
    #Now apply returns
    stepRtns <- rtns[projIndex, , drop = FALSE]
    
    endAssets <- assetsPostRegFlow * (1 + stepRtns)
    
    stepParams$startYear <- startYear + 1
    stepParams$outputList$potSize <- endAssets
    stepParams$outputList$actualRegularFlowsOut <- actualRegularFlowsOut
    return(stepParams)
  }
  
  funcall <- function(params, f) f(params)
  
  allSimResults <-
    purrr::accumulate(
      rep.int(list(stepFn), nproj),
      funcall,
      .init = initParams
    )
  
  #Need to turn list of output lists into output list of lists
  transposedOutput <-
    purrr::transpose(purrr::map(allSimResults, "outputList"))
  
  #And then create list of matrices to output
  #Drop is there in case single element output is included
  purrr::map(transposedOutput, ~ drop(do.call(rbind, .x)))
}


microbenchmark::microbenchmark({
  potProjection_r <- 
    projectDrawdownPot(initialPot = initialPot, rtns = rtns, 
      targetRegularFlowsOut = targetRegularFlowsOut)
})

Rcpp::sourceCpp("test.cpp")
microbenchmark::microbenchmark({
  potProjection_cpp <- 
    projectDrawdownPotCpp(initialPot = initialPot, rtns = rtns, 
      targetRegularFlowsOut = targetRegularFlowsOut)
})
