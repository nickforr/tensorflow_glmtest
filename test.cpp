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
