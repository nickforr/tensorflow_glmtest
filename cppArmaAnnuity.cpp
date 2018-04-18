#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
arma::mat armaPriceAnnuity(const arma::mat& yieldData, const arma::vec& paymentProbs) {
  
  int nproj = yieldData.n_rows;
  int nsim = yieldData.n_cols;
  
  arma::mat annuityPrices(nproj, nsim, arma::fill::zeros);
  
  for (int iproj = 0; iproj < nproj; ++iproj) {
    
    arma::vec combinedProbs(nproj - iproj);
    
    if (iproj == 0) {
      combinedProbs = paymentProbs;
    } else {
      combinedProbs = 
        paymentProbs.subvec(iproj, nproj - 1) / paymentProbs[iproj];
    }
    
    arma::vec logYields = arma::log(1.0 + yieldData.row(iproj));
    arma::rowvec maturities = 
      -arma::cumsum(arma::rowvec(nproj, arma::fill::ones));
    arma::mat projYields = arma::exp(logYields * maturities);
    arma::vec price = projYields * combinedProbs;
    
    annuityPrices.row(iproj) = price;
  }
  return annuityPrices;
}

