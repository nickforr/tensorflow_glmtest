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
  
  assets(0, _) = pot;
  
  for (int iproj = 1; iproj < nproj; ++iproj) {
    
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
