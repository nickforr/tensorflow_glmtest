

priceAnnuityR <- function(paymentProbsA, yieldData) {
  
  
  paymentTiming <- match.arg(paymentTiming)
  timingAdjustment <- switch(paymentTiming, start = 0, end = 1)
  
  annuityPrices <- yieldData * 0 #use yieldData to get 0 matrix of right dims
  
  for (i in seq_len(nrow(annuityPrices))) {
    
    probsA <- #payment probs without gtee
      if (i == 1) {
        paymentProbsA
      } else {
        utils::tail(paymentProbsA, -i + 1) / paymentProbsA[i]
      }
    
    combinedProbs <- probsA
    
    if (gteeYrs > 0) {
      probsA_gtee <- probsA
      #Automatically extends probsA_gtee if length shorter than gteeYrs
      probsA_gtee[seq_len(gteeYrs)] <- 1
      combinedProbs <- probsA_gtee
    }
    
    if (!is.null(paymentProbsB)) {
      
      probsB <-
        if (i == 1) {
          paymentProbsB
        } else {
          probScalingFactor <-
            if (lifeBInitialSurvivalAssumed) {
              paymentProbsB[i]
            } else {
              1
            }
          utils::tail(paymentProbsB, -i + 1) / probScalingFactor
        }
      
      #need to make sure probsA and probsB are the same length to avoid
      #recycling when multiplying them
      if (length(probsA) < length(probsB)) {
        probsA <- c(probsA, rep(0, length(probsB) - length(probsA)))
      }
      else if (length(probsA) > length(probsB)) {
        probsB <- c(probsB, rep(0, length(probsA) - length(probsB)))
      }
      
      #need to use probsA here as we need to exclude the gtee
      reversionaryProbs <- (1 - probsA) * probsB
      
      #need to make sure combinedProbs and reversionaryProbs are the same length
      #to avoid recycling when multiplying them
      if (length(combinedProbs) < length(reversionaryProbs)) {
        combinedProbs <-
          c(combinedProbs,
            rep(0, length(reversionaryProbs) - length(combinedProbs)))
      }
      else if (length(combinedProbs) > length(reversionaryProbs)) {
        reversionaryProbs <-
          c(reversionaryProbs,
            rep(0, length(combinedProbs) - length(reversionaryProbs)))
      }
      
      combinedProbs <- combinedProbs + spousePpn * reversionaryProbs
    }
    
    yieldSims <- yieldData[i, ]
    logYields <- log(1 + yieldSims)
    maturities <- -(seq_along(combinedProbs) - 1 + timingAdjustment)
    projYields <- exp(tcrossprod(logYields, maturities))
    price <- projYields %*% combinedProbs
    annuityPrices[i, ] <- price
  }
  annuityPrices
}



yieldData <- matrix(rnorm(71 * 5000, mean = 0.03, sd = 0.02), nrow = 71)
paymentProbsA <- rev(seq_len(71)) / 71

microbenchmark::microbenchmark({
  annuityPrices_R <- 
    priceAnnuityPayments(paymentProbsA = paymentProbsA, yieldData = yieldData)
}, times = 20)


Rcpp::sourceCpp("cppAnnuity.cpp")
microbenchmark::microbenchmark({
  annuityPrices_cpp <- priceAnnuityCpp(yieldData, paymentProbsA, 0)
}, times = 50)
# Check this equals initial R implementation
all.equal(annuityPrices_R, annuityPrices_cpp)


