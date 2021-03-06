//--------------------//
// LarryHua.com, 2019 //
//--------------------//

#include <Rcpp.h>
using namespace Rcpp;

//' time index that triggers a symmetric CUSUM filter
//'
//' @param x a vector of time series to be filtered
//' @param h a vector of the thresholds
//'
//' @examples
//' set.seed(1)
//' x <- runif(100, 1, 3)
//' h <- rep(1.5, 100)
//' i_CUSUM <- istar_CUSUM(x,h)
//' abline(v=i_CUSUM, lty = 2)
//'
//' ## Comparing C and R versions
//' # set.seed(1)
//' # x <- runif(1000000, 1, 3)
//' # h <- rep(1.5, 100)
//'
//' # start_time <- Sys.time()
//' # i_CUSUM <- istar_CUSUM(x,h)
//' # end_time <- Sys.time()
//' # C_time <- end_time - start_time
//'
//' # start_time <- Sys.time()
//' # i_CUSUM_R <- istar_CUSUM_R(x,h)
//' # end_time <- Sys.time()
//' # R_time <- end_time - start_time
//' # cat("C and R time: ", C_time, R_time)
//' # all(i_CUSUM-i_CUSUM_R==0)
//' 
//' @author Larry Lei Hua
//' 
//' @export
// [[Rcpp::export]]
IntegerVector istar_CUSUM(NumericVector x, NumericVector h){
    const int nx = x.size();
    NumericVector xminusEx = diff(x);
    double S_pos = 0.0;
    double S_neg = 0.0;
    IntegerVector istar;
    int i;
    
    if(h.size() == 1){
      for( i=0; i<nx-1; ++i){
        h.push_back(h[0]);
      }
    }

    // -2: because istar_CUSUM cannot be the last time index
    // otherwise there is no bar afterwards for labeling
    for( i=0; i < nx-2; ++i )
    {
      S_pos = std::max(0.0, S_pos + xminusEx[i]);
      S_neg = std::min(0.0, S_neg + xminusEx[i]);
      if(std::max(S_pos, -S_neg) >= h[i])
      {
        istar.push_back(i+2);
        S_pos = 0.0;
        S_neg = 0.0;
      }
    }
    return istar;
}
