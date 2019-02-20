#--------------------#
# LarryHua.com, 2019 #
#--------------------#

#' time index that triggers a symmetric CUSUM filter (R version for istar_CUSUM())
#' 
#' @param x a vector of time series to be filtered
#' @param h a vector of the thresholds
#' 
#' @export
istar_CUSUM_R <- function(x, h)
{
  nx <- length(x)
  xminusEx <- diff(x)
  
  S_pos <- S_neg <- 0
  istar <- NULL
  
  # -2: because istar_CUSUM cannot be the last time index
  # otherwise there is no bar afterwards for labeling
  for(i in 1:(nx-2))
  {
    S_pos <- max(0, S_pos + xminusEx[i])
    S_neg <- min(0, S_neg + xminusEx[i])
    if(max(S_pos, -S_neg) >= h[i])
    {
      istar <- c(istar, i+1) # +1 because the 1st diff() arises from the 2nd time step
      S_pos <- S_neg <- 0
    }
  }
  return(istar)
}
