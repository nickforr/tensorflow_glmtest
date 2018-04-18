#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
arma::mat armaPriceAnnuity(const arma::mat& yieldData, const arma::rowvec& paymentProbs) {
  
  int nproj = yieldData.n_rows;
  int nsim = yieldData.n_cols;
  
  arma::mat annuityPrices(nproj, nsim, arma::fill::zeros);
  
  for (int iproj = 0; iproj < nproj; ++iproj) {
    
    arma::rowvec combinedProbs(nproj - iproj);
    
    if (iproj == 0) {
      combinedProbs = paymentProbs;
    } else {
      combinedProbs = 
        paymentProbs.subvec(iproj, nproj - 1) / paymentProbs[iproj];
    }
    
    arma::rowvec logYields = arma::log(1.0 + yieldData.row(iproj));
    arma::vec maturities = 
      -(arma::cumsum(arma::vec(nproj - iproj, arma::fill::ones)) - 1.0);
    arma::mat projYields = arma::exp(maturities * logYields);
    arma::rowvec price = combinedProbs * projYields;
    
    annuityPrices.row(iproj) = price;
  }
  return annuityPrices;
}

