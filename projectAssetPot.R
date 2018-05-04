

#' Project Asset Pot
#' 
#' Simple projection function applying returns to an initial pot and, or, some
#' net flows.
#' 
#' Note that a positive `flow` is assumed to be money coming into the assets,
#' whereas a negative `flow` represents money being paid out of the asset pot.
#' 
#' Note that a positive `adjustment` is assumed to be money coming into the assets,
#' whereas a negative `adjustment` represents money being paid into the asset pot.
#' 
#' The `rtns` argument comes first in the function to enable easy piping from
#' the creation of portfolio returns.
#' 
#' `initialPot` can be a vector (of same length as the number of simulations in
#' `rtns`) representing, for example, a simulated start point.
#' 
#' Both `flows` and `adjustments` can be single numbers, vectors (over time) or
#' matrices, and the last element (or row) will be automatically extended to
#' cover the projection period.
#' 
#' Note that the assets are floored at zero if flows are coming out (i.e. no
#' payments out can be paid but money can still be paid in).
#'
#' @param rtns a matrix of returnsto use
#' @param initialPot numeric (scalar) vector (if non-scalar, must be same length
#'   as number of simulations in `rtns`)
#' @param flows a numeric amount that is to be paid into (positive) or out of
#'   (negative) the assets. Can be one of:
#' \itemize{ 
#'   \item single amount used in every timestep
#'   \item a vector of varying amounts, with the last amount extended for the
#'   remainder of the projection period
#'   \item a matrix of varying amounts, with the last row extended for the
#'   remainder of the projection period (must have same `ncol` as `rtns`)
#' }
#' @param flowTiming numeric argument defining when in timestep flow occurs
#'   (default is halfway through the year)
#' @param adjustments numeric amount that works in a similar way to `flows` but
#'   allows function to adjust assets separately, e.g. if there are overlays
#'   calculated elsewhere). Can be one of:
#' \itemize{ 
#'   \item single amount used in every timestep
#'   \item a vector of varying amounts, with the last amount extended for the
#'   remainder of the projection period
#'   \item a matrix of varying amounts, with the last row extended for the
#'   remainder of the projection period (must have same `ncol` as `rtns`)
#' }
#' @param adjustmentsTiming numeric argument defining when in timestep
#'   adjustment occurs (default is at the end of the year)
#' @param outputAssetOnly logical argument which determines whether just the
#'   asset matrix is returned or whether the bigger list of output matrices are
#'   returned
#'
#' @return either an output asset matrix or a a list of output matrices. One for
#'   `assets` and one for `flowsPaid`. These have same dimension as `rtns`.
#' @export
#'
#' @examples
#' rtns <- matrix(rnorm(11 * 50, 0.04, 0.15), nrow = 11)
#' rtns[1, ] <- 0
#' initialPot <- 100
#' flows <- 5
#' assets <- projectAssetPot(rtns, initialPot, flows)
projectAssetPot <- function(rtns, initialPot = 0, flows = 0, flowTiming = 0.5, 
                            adjustments = NULL, adjustmentsTiming = 1, 
                            outputAssetOnly = TRUE, useCpp = FALSE) {
  
  #validateSimMatrixArguments(rtns)
  nsim <- ncol(rtns)
  nproj <- nrow(rtns) - 1
  
  if (!is.numeric((initialPot))) {
    stop("initialPot must be numeric")
  }
  pot <- 
    if (length(initialPot) > 1) {
      if (length(initialPot) != nsim) {
        stop("If length of initialPot is greater than 1, it must equal number or simulations")
      }
      initialPot
    }
  else {
    rep(initialPot, nsim)
  }
  
  #Extend flows
  fullFlows <- extendWtsAndFlows(flows, nproj + 1)
  
  #Check flowTiming
  if (flowTiming > 1 || flowTiming < 0) {
    stop("flowTiming must be between 0 and 1, inclusive")
  }
  
  #Extend adjustments
  fullAdjustments <- 
    if (is.null(adjustments)) {
      extendWtsAndFlows(0, nproj + 1)
    } else {
      extendWtsAndFlows(adjustments, nproj + 1)
    }
  #Check adjustmentsTiming
  if (adjustmentsTiming > 1 || adjustmentsTiming < 0) {
    stop("adjustmentsTiming must be between 0 and 1, inclusive")
  }
  
  matrixList <- 
    if (useCpp) {
      stop("no cpp yet")
      projectPot_cpp()
    } else {
      projectPot_r(
        pot, rtns, nsim, nproj, 
        fullFlows, flowTiming, 
        fullAdjustments, adjustmentsTiming)
    }
  
  if (outputAssetOnly) {
    matrixList[["assets"]]
  } else {
    matrixList
  }
}


Rcpp::sourceCpp("projectAssetPotCpp.cpp")

projectPot_r <- function(pot, rtns, nsim, nproj, fullFlows, flowTiming, 
                          fullAdjustments, adjustmentsTiming) {
  
  #Set initial parameters
  initParams <-
    list(
      startYear = 0,
      outputList = 
        list(
          assets = pot,
          flowsPaid = rep(NA, nsim)
        )
    )
  
  stepFn <- function(stepParams) {
    
    startYear = stepParams$startYear
    projIndex <- startYear + 2
    startYrIndex <- startYear + 1
    
    startAssets <- stepParams$outputList$assets
    
    #Returns
    stepRtns <- rtns[projIndex, , drop = FALSE]
    
    #flows
    stepFlow <- 
      if (is.null(dim(fullFlows))) {
        fullFlows[projIndex]
      } else{
        fullFlows[projIndex, , drop = FALSE] 
      }
    
    #adjustments
    stepAdjustments <- 
      if (is.null(dim(fullAdjustments))) {
        fullAdjustments[projIndex]
      } else{
        fullAdjustments[projIndex, , drop = FALSE] 
      }
    
    assetsPreFlow <- 
      if (adjustmentsTiming < flowTiming) {
        (startAssets * (1 + stepRtns) ^ adjustmentsTiming 
          + stepAdjustments) ^ (flowTiming - adjustmentsTiming)
      } else {
        startAssets * (1 + stepRtns) ^ flowTiming
      }
    
    #Create logical vector based on whether assets have run out or not
    assetsNotRunOut <- (assetsPreFlow > 0)
    
    #Create logical vector based on whether flows are +ve or -ve
    flowNegative <- (stepFlow < 0)
    
    flowsPaid <- 
      flowNegative * -pmin(assetsNotRunOut * assetsPreFlow, -stepFlow) + 
      (!flowNegative) * stepFlow
    
    assetsPostFlow <- assetsPreFlow + flowsPaid
    
    endAssets <- 
      if (adjustmentsTiming < flowTiming) {
        assetsPostFlow * (1 + stepRtns) ^ (1 - flowTiming)
      } else {
        (assetsPostFlow * (1 + stepRtns) ^ (adjustmentsTiming - flowTiming)
          + stepAdjustments) * (1 + stepRtns) ^ (1 - adjustmentsTiming)
      }
    
    stepParams$startYear <- startYear + 1
    stepParams$outputList$assets <- endAssets
    stepParams$outputList$flowsPaid <- flowsPaid
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
  
  #And then create list of matrices
  #Drop is there in case single element output is included
  purrr::map(transposedOutput, ~ drop(do.call(rbind, .x)))
  
}



# Extend weights & flows fn
extendWtsAndFlows <- function(x, totalRows, addZeroFirst = TRUE) {
  
  if (is.null(x)) return(NULL)
  if (totalRows <= 0) stop("totalRows must be greater than 0")
  
  indices <-
    if (addZeroFirst) {
      seq_len(totalRows - 1) #1 less because start with 0 element
    } else {
      seq_len(totalRows) #not adding 0 element
    }
  xLength <- 
    if (is.matrix(x)) {
      nrow(x)
    } else {
      length(x) 
    }
  interimResult <-
    if (is.matrix(x)) {
      x[pmin.int(indices, xLength), , drop = FALSE]
    } else {
      x[pmin.int(indices, xLength)]
    }
  if (addZeroFirst) {
    if (is.matrix(x)) {
      rbind(
        rep.int(0, ncol(x)),
        interimResult
      )
    } else {
      c(0, interimResult)
    }
  } else {
    interimResult
  }
}

