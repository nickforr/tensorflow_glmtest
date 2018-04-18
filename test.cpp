#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat mmultTest(const arma::vec& x, const arma::vec& y) {
  arma::mat a = x * y.t();
  return a;
}

// [[Rcpp::export]]
arma::mat mmultTest2(const arma::vec& x, const arma::vec& y) {
  
  int nproj = y.size();
  arma::rowvec maturities = 
    arma::cumsum(arma::rowvec(nproj, arma::fill::ones));
    
  arma::mat a = x * maturities;
  return a;
}

// [[Rcpp::export]]
arma::mat matTests(const arma::mat& yieldData) {
  
  int nproj = yieldData.n_rows;
  int nsim = yieldData.n_cols;
  
  arma::mat annuityPrices(nproj, nsim, arma::fill::zeros);
  
  for (int iproj = 0; iproj < nproj; ++iproj) {
    
    arma::rowvec x(nproj);
    
    x = yieldData.row(iproj) - 1.0;
    
    annuityPrices.row(iproj) = x;
  }
  return annuityPrices;
}

