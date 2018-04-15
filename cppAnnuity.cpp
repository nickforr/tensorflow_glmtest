#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix priceAnnuityCpp(NumericMatrix yieldData, 
                              NumericVector paymentProbs) {
  
  int nproj = yieldData.nrow();
  int nsim = yieldData.ncol();
  NumericMatrix annuityPrice(nproj, nsim);
  
  for (int iproj = 0; iproj < nproj; iproj++) {
    
    NumericVector loopProbs;
    
    if (iproj == 0) {
      loopProbs = paymentProbs;
    } else {
      loopProbs = tail(paymentProbs, -iproj) / paymentProbs[iproj];
    }
    
    for (int isim = 0; isim < nsim; isim++) {
      double yld = yieldData(iproj, isim);
      for (int iiproj = 0; iiproj < loopProbs.size(); iiproj++) {
        annuityPrice(iproj, isim) = 
          annuityPrice(iproj, isim) + 
          loopProbs[iiproj] * std::pow(1.0 + yld, -iiproj);  
      }
    }
  }
  return annuityPrice;
}

