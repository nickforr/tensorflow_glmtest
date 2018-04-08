#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix goProjectCbnsCpp(NumericVector cbnRate, 
                                NumericMatrix salProjection ,
                                NumericMatrix rtns) {
  
  int nproj = cbnRate.size();
  int nsim = salProjection.ncol();
  NumericMatrix cbnProjection(nproj, nsim);
  
  for (int iproj = 1; iproj < nproj; iproj++) {
    
    NumericVector cbn = cbnRate[iproj] * salProjection(iproj - 1,_);
    cbnProjection(iproj,_) = 
      cbnProjection(iproj - 1, _) * (1 + rtns(iproj, _)) + cbn;
  }
  return cbnProjection;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
goProjectCbnsCpp(
  cbnRate = c(0.05, 0.05), 
  salProjection = matrix(1, nrow = 2, ncol = 4), 
  rtns = matrix(0.01, nrow = 2, ncol = 4))
*/
