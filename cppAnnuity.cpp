#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix priceAnnuityCpp(NumericMatrix yieldData, 
  NumericVector paymentProbs, 
  NumericVector timingAdjustment) {
  
  int nproj = yieldData.nrow();
  int nsim = yieldData.ncol();
  NumericMatrix annuityPrice(nproj, nsim);
  
  for (int iproj = 0; iproj < nproj; iproj++) {
    
    NumericVector loopProbs;
    
    if (iproj == 0) {
      loopProbs = paymentProbs;
    } else {
      loopProbs = tail(paymentProbs, -iproj) / paymentProbs[iproj - 1];
    }
    
    for (int iiproj = 0; iiproj < loopProbs.size(); iiproj++) {
      for (int isim = 0; isim < nsim; isim++) {
        double yld = yieldData(iproj, isim);
        double maturity = iiproj + timingAdjustment(0);
        annuityPrice(iproj, isim) = 
          annuityPrice(iproj, isim) + 
          loopProbs[iiproj] * pow(1 + yld, -maturity);  
      }
    }
  }
  return annuityPrice;
}

