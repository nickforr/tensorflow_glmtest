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
  Rcpp::NumericVector assetsPreFlowFloored(nsim);
  Rcpp::LogicalVector flowNegative(nsim);
  Rcpp::NumericVector assetsPostFlow(nsim);
  
  assets(0, _) = pot;
  
  for (unsigned int iproj = 1; iproj < nproj; ++iproj) {
    
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
    // Floor assets at zero
    assetsPreFlowFloored = Rcpp::pmax(assetsPreFlow, 0.0);
    
    // Create logical vector based on whether flows are +ve or -ve
    flowNegative = flows(iproj, _) < 0;
    
    for (unsigned int isim = 0; isim < nsim; ++isim) {
      
      if (flowNegative[isim]) {
        flowsPaid(iproj, isim) = 
          -std::min(assetsPreFlowFloored[isim], -flows(iproj, isim));
      } else {
        flowsPaid(iproj, isim) = flows(iproj, isim);
      }
    }
    
    assetsPostFlow = assetsPreFlowFloored + flowsPaid(iproj, _);
    
    if (adjustmentsTiming < flowTiming) {
      assets(iproj, _) = 
        assetsPostFlow * 
        Rcpp::pow(1.0 + rtns(iproj, _), 1.0 - flowTiming);
    } else {
      assets(iproj, _) = 
        (assetsPostFlow * 
        Rcpp::pow(1.0 + rtns(iproj, _), adjustmentsTiming - flowTiming)+ 
        fullAdjustments(iproj, _)) * 
        Rcpp::pow(1.0 + rtns(iproj, _), 1.0 - adjustmentsTiming); 
    }
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
flows <- matrix(-5, nrow = 10, ncol = 100)
flowTiming <- 0.5
adjustments <- matrix(1, nrow = 10, ncol = 100)
adjustmentsTiming <- 1
ans <- 
  projectPot_cpp(pot, rtns, flows, flowTiming, adjustments, adjustmentsTiming)
*/
