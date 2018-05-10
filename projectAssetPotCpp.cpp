#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List projectPot_cpp(
    Rcpp::NumericVector pot, 
    Rcpp::NumericMatrix rtns, 
    Rcpp::NumericMatrix flows, 
    double flowTiming, 
    Rcpp::NumericMatrix fullAdjustments, 
    double adjustmentsTiming) {
  
  const unsigned int nsim = rtns.ncol();
  const unsigned int nproj = rtns.nrow(); 
  
  Rcpp::NumericMatrix assets(nproj, nsim);
  Rcpp::NumericMatrix flowsPaid(nproj, nsim);
  Rcpp::NumericVector assetsPreFlow(nsim);
  
  assets(0, _) = pot;
  
  for (int iproj = 1; iproj < nproj; ++iproj) {
    
    if (adjustmentsTiming < flowTiming) {
      assetsPreFlow = 
        (assets(iproj - 1, _) * 
        Rcpp::pow(1.0 + rtns(iproj, _), adjustmentsTiming) + 
        fullAdjustments(iproj, _)) * 
        Rcpp::pow(1.0 + rtns(iproj, _), flowTiming - adjustmentsTiming);
    } else {
      assetsPreFlow = 
        assets(iproj - 1, _) * 
        Rcpp::pow(1.0 + rtns(iproj, _), flowTiming); 
    }
    
// Create logical vector based on whether assets have run out or not    
    Rcpp::LogicalVector assetsNotRunOut = assetsPreFlow > 0.0;
        
// Create logical vector based on whether flows are +ve or -ve
    Rcpp::LogicalVector flowNegative = flows(iproj, _) < 0;
          
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
    
    flowsPaid(iproj, _) = 
      pmin(potSize(iproj - 1, _), targetRegularFlowsOut(iproj, _));
    
    potSize(iproj, _) = 
      pmax(0.0, potSize(iproj - 1, _) - regularFlowsPaid(iproj, _)) * 
      (1.0 + rtns(iproj, _));
  }
  return Rcpp::List::create(
    Rcpp::Named("pot") = assets,
    Rcpp::Named("flowsPaid") = flowsPaid);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
rtns <- matrix(rnorm(100 * 10, mean = 0.04, sd = 0.1), nrow = 10)
pot <- rep.int(100, 100)
flows <- rep.int(-5, 10)
flowTiming <- 0.5
adjustments <- rep.int(1, 10)
adjustmentsTiming <- 1
projectPot_cpp(pot, rtns, flows, flowTiming, adjustments, adjustmentsTiming)
*/
