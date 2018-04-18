#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
arma::mat armaPriceAnnuity(const arma::mat& yieldData, const arma::vec& paymentProbs) {
  
  int nproj = yieldData.n_rows;
  int nsim = yieldData.n_cols;
  
  arma::mat annuityPrices(nproj, nsim, arma::fill::zeros);
  
  for (int iproj = 0; iproj < nproj; ++iproj) {
    
    if (iproj == 0) {
      arma::vec combinedProbs = paymentProbs;
    } else {
      arma::vec combinedProbs = 
        paymentProbs.subvec(iproj, nproj - 1) / paymentProbs[iproj];
    }
    
    arma::vec logYields = arma::log(1.0 + yieldData.row(iproj));
    arma::vec maturities = 
      -arma::cumsum(arma::vec(nproj - 1, arma::fill::ones));
    arma::mat projYields = arma::exp(logYields * arma::trans(maturities));
    arma::vec price = projYields * combinedProbs;
    
    annuityPrices.row(iproj) = price;
  }
  return annuityPrices;
}

