#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List projectDrawdownPotCpp(const double& initialPot, 
  const NumericMatrix& rtns, 
  const NumericMatrix& targetRegularFlowsOut) {
  
  int nsim = rtns.ncol();
  int nproj = rtns.nrow(); 
  
  NumericMatrix potSize(nproj, nsim);
  NumericMatrix regularFlowsPaid(nproj, nsim);
  
  NumericVector pot(nsim, initialPot);
  
  potSize(0, _) = pot;
  
  for (int iproj = 1; iproj < nproj; ++iproj) {
    
    regularFlowsPaid(iproj, _) = 
      pmin(potSize(iproj - 1, _), targetRegularFlowsOut(iproj, _));
    
    potSize(iproj, _) = 
      pmax(0.0, potSize(iproj - 1, _) - regularFlowsPaid(iproj, _)) * 
      (1.0 + rtns(iproj, _));
  }
  return Rcpp::List::create(
    Rcpp::Named("pot") = potSize,
    Rcpp::Named("flowsPaid") = regularFlowsPaid);
}

