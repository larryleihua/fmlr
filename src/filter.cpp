#include <Rcpp.h>
using namespace Rcpp;

//' time index that triggers a symmetric CUSUM filter
//'
//' @param x a vector of time series to be filtered
//' @param h a positive number for the thresholds
//'
//' @examples
//' set.seed(1)
//' x <- runif(100, 1, 3)
//' h <- 1.5
//' i_CUSUM <- istar_CUSUM(x,h)
//' plot(x)
//' abline(v=i_CUSUM, lty = 2)
//'
//' ## Comparing C and R versions
//' set.seed(1)
//' x <- runif(1000000, 1, 3)
//' h <- 1.5
//'
//' start_time <- Sys.time()
//' i_CUSUM <- istar_CUSUM(x,h)
//' end_time <- Sys.time()
//' C_time <- end_time - start_time
//'
//' start_time <- Sys.time()
//' i_CUSUM_R <- istar_CUSUM_R(x,h)
//' end_time <- Sys.time()
//' R_time <- end_time - start_time
//' cat("C and R time: ", C_time, R_time)
//' all(i_CUSUM-i_CUSUM_R==0)
//' @export
// [[Rcpp::export]]
IntegerVector istar_CUSUM(NumericVector x, double h){
    const int nx = x.size();
    NumericVector xminusEx = diff(x);
    double S_pos = 0.0;
    double S_neg = 0.0;
    IntegerVector istar;
    for( int i=0; i < nx-1; ++i )
    {
      S_pos = std::max(0.0, S_pos + xminusEx[i]);
      S_neg = std::min(0.0, S_neg + xminusEx[i]);
      if(std::max(S_pos, -S_neg) >= h)
      {
        istar.push_back(i+2);
        S_pos = 0.0;
        S_neg = 0.0;
      }
    }
    return istar;
}
