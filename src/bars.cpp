#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
double ema(NumericVector x, int n) {
  const int nx = x.size();
  double out = x[0];
  double a = 2/((double)n+1);

  for(int i = 1; i < nx; ++i) {
    out = a*x[i] + (1-a)*out;
  }   
  return out;	
}

//' Tstar index for Tick Runs Bars (bar_trb)
//' @param b_t output of imbalance_tick(dat) with the dat has at least the following columns: Price
//' @param w0 the time window length of the first bar
//' @param bkw_T backward window length for exponentially weighted average T
//' @param bkw_Pb1 backward window length for exponentially weighted average P[b_t=1]
//' 
//' @return a list of the following two vectors: 
//'     a vector for the lengths of the tick imbalance bars. For example, if the return is c(10,26), then the 2 tick imbalance bars are (0,10] and (10, 36]
//'     a vector indicating up runs or down runs
//' 
//' @examples
//' 
//' set.seed(1)
//' dat <- data.frame(Price = c(rep(0.5, 5), runif(100)))
//' b_t <- imbalance_tick(dat)
//' T_trb <- Tstar_trb_cpp(b_t, 10, 10, 10)
//' col <- ifelse(T_trb$Type==1, "red", "blue")
//' T <- cumsum(T_trb$Tstar)
//' plot(dat$Price)
//' for(i in 1:length(T)) abline(v = T[i], col = col[i])
//'  
//' @export
// [[Rcpp::export]]
List Tstar_trb_cpp(IntegerVector b_t, int w0, int bkw_T, int bkw_Pb1)
{
  const int nb = b_t.size();
  int i, sum_pos, sum_neg, T_new, T_last=0;
  double Pb1, Pb1_new;
  
  // Tvec: the vector to store the time index
  IntegerVector Tvec;
  
  // Rvec: the vector to store the type of runs: up/down runs
  IntegerVector Rvec;

  // Pb1vec: the vector to store P[b_t=1]
  NumericVector Pb1vec;
  
  // assuming E0T = w0 and Pb1=0.5 to simplify the logic for the first bar
  double E0T=(double)w0;
  double th_T_Expected = E0T*(0.5);
    
  while( T_last<nb-1 ) {
    
    sum_pos = 0;
    sum_neg = 0;

    for( i=0; i<nb-T_last; ++i ) {

      if(b_t[T_last+i] == 1) {
        ++sum_pos;
      } else if (b_t[T_last+i] == -1) {
        ++sum_neg;
      }
      
      if(sum_pos >= th_T_Expected) {
        T_new = i + 1;
        Tvec.push_back(T_new);
        Rvec.push_back(1);
        
        Pb1_new = (double)sum_pos / (double)T_new;
        Pb1vec.push_back(Pb1_new);
        E0T = ema(as<NumericVector>(Tvec), bkw_T);
        Pb1 = ema(Pb1vec, bkw_Pb1);
        T_last += T_new;
        th_T_Expected = E0T*max(Pb1, 1-Pb1);
        break;
        
      } else if(sum_neg >= th_T_Expected) {
        
        T_new = i + 1;
        Tvec.push_back(T_new);
        Rvec.push_back(-1);
        
        Pb1_new = (double)sum_pos / (double)T_new;
        Pb1vec.push_back(Pb1_new); 
        E0T = ema(as<NumericVector>(Tvec), bkw_T);
        Pb1 = ema(Pb1vec, bkw_Pb1);
        T_last += T_new;
        th_T_Expected = E0T*max(Pb1, 1-Pb1);
        break;
      }
    }
    if(i >= nb-T_last) break; // there are no bars left, stop the while loop
  }
  return List::create(Named("Tstar") = Tvec, Named("Type") = Rvec);
}

//' Tstar index for Volume Runs Bars (bar_vrb)
//' @param b_t output of imbalance_tick(dat) with the data 'dat' has at least the following columns: Price
//' @param v_t volume of the same data
//' @param v_0 average volume for each trade, and it is used to create the first bar
//' @param w0 the time window length of the first bar
//' @param bkw_T backward window length for exponentially weighted average T
//' @param bkw_Pb1 backward window length for exponentially weighted average P[b_t=1]
//' @param bkw_V backward window length for exponentially weighted average volumes
//' 
//' @return a list of the following two vectors: 
//'     a vector for the lengths of the tick imbalance bars. For example, if the return is c(10,26), then the 2 tick imbalance bars are (0,10] and (10, 36]
//'     a vector indicating up runs or down runs
//' 
//' @examples
//' 
//' set.seed(1)
//' dat <- data.frame(Price = c(rep(0.5, 5), runif(100)), Size = runif(105, 10, 100))
//' b_t <- imbalance_tick(dat)
//' v_t <- dat$Size
//' T_vrb <- Tstar_vrb_cpp(b_t, v_t, 55, 10, 10, 10, 10)
//' col <- ifelse(T_vrb$Type==1, "red", "blue")
//' T <- cumsum(T_vrb$Tstar)
//' plot(dat$Price)
//' for(i in 1:length(T)) abline(v = T[i], col = col[i])
//'  
//' @export
//' 
// [[Rcpp::export]]
List Tstar_vrb_cpp(IntegerVector b_t, NumericVector v_t, double v_0, int w0, int bkw_T, int bkw_Pb1, int bkw_V)
{
  const int nb = b_t.size();
  int i, sum_pos, sum_neg, T_new, T_last=0;
  double Pb1, Pb1_new;
  double sum_v_pos, sum_v_neg;
  
  // Tvec: the vector to store the time index
  IntegerVector Tvec;
  
  // Rvec: the vector to store the type of runs: up/down runs
  IntegerVector Rvec;
  
  // Pb1vec: the vector to store P[b_t=1]
  NumericVector Pb1vec;
  
  // avg_v_pos / avg_v_neg: the vector to average volume
  NumericVector avg_v_pos, avg_v_neg;

  // assuming E0T = w0 and Pb1=0.5, and use the average volume v_0 to simplify the logic for the first bar
  double E0T = (double)w0;
  double E0V = v_0;
  double th_T_Expected = E0T*E0V*(0.5);
  
  double E0V_pos, E0V_neg;
  
  while( T_last<nb-1 ) {
    
    sum_pos = 0;
    sum_neg = 0;
    sum_v_pos = 0.0;
    sum_v_neg = 0.0;

    for( i=0; i<nb-T_last; ++i ) {
      
      if(b_t[T_last+i] == 1) {
        ++sum_pos;
        sum_v_pos += v_t[T_last+i];
      } else if (b_t[T_last+i] == -1) {
        ++sum_neg;
        sum_v_neg += v_t[T_last+i];
      }
      
      if(sum_v_pos >= th_T_Expected) {
        T_new = i + 1;
        Tvec.push_back(T_new);
        Rvec.push_back(1);

        Pb1_new = (double)sum_pos / (double)T_new;
        Pb1vec.push_back(Pb1_new);
        
        if(sum_pos != 0) {
          avg_v_pos.push_back( sum_v_pos/(double)sum_pos );
        } else {
          avg_v_pos.push_back(0.0);
        }
        
        if(sum_neg != 0) {
          avg_v_neg.push_back( sum_v_neg/(double)sum_neg );
        } else {
          avg_v_neg.push_back(0.0);
        }

        E0V_pos = ema(avg_v_pos, bkw_V);
        E0V_neg = ema(avg_v_neg, bkw_V);
        E0T = ema(as<NumericVector>(Tvec), bkw_T);
        Pb1 = ema(Pb1vec, bkw_Pb1);

        T_last += T_new;
        th_T_Expected = E0T*max(Pb1*E0V_pos, (1-Pb1)*E0V_neg);
        break;
        
      } else if(sum_v_neg >= th_T_Expected) {
        
        T_new = i + 1;
        Tvec.push_back(T_new);
        Rvec.push_back(-1);
        
        Pb1_new = (double)sum_pos / (double)T_new;
        Pb1vec.push_back(Pb1_new);
        
        if(sum_pos != 0) {
          avg_v_pos.push_back( sum_v_pos/(double)sum_pos );
        } else {
          avg_v_pos.push_back(0.0);
        }
        
        if(sum_neg != 0) {
          avg_v_neg.push_back( sum_v_neg/(double)sum_neg );
        } else {
          avg_v_neg.push_back(0.0);
        }
        
        E0V_pos = ema(avg_v_pos, bkw_V);
        E0V_neg = ema(avg_v_neg, bkw_V);
        E0T = ema(as<NumericVector>(Tvec), bkw_T);
        Pb1 = ema(Pb1vec, bkw_Pb1);
        
        T_last += T_new;
        th_T_Expected = E0T*max(Pb1*E0V_pos, (1-Pb1)*E0V_neg);
        break;
      }
    }
    if(i >= nb-T_last) break; // there are no bars left, stop the while loop
  }
  return List::create(Named("Tstar") = Tvec, Named("Type") = Rvec);
}

