#--------------------#
# LarryHua.com, 2019 #
#--------------------#

#' calculate the weights for deriving fractionally differentiated series
#'
#' @param d the order for fractionally differentiated features
#' @param nWei number of weights for output
#' @param tau threshold where weights are cut off; default is NULL, if not NULL then use tau and nWei is not used
#'
#' @examples
#' weights_fracDiff(0.5,tau=1e-3)
#' 
#' @author Larry Lei Hua
#'
#' @export
weights_fracDiff <- function(d=0.5, nWei=10, tau=NULL)
{
  wVec <- w0 <- 1
  if(is.null(tau))
  {
    for(k in 1:(nWei-1))
    {
      w1 <- (-1)*w0*(d-k+1)/k
      wVec <- c(wVec, w1)
      w0 <- w1
    }
  }else
  {
    k <- 1
    while(abs(w0) >= tau)
    {
      w1 <- (-1)*w0*(d-k+1)/k
      wVec <- c(wVec, w1)
      w0 <- w1
      k <- k+1
    }
    wVec <- wVec[-length(wVec)] # remove the last one which is already < tau
  }
  return(wVec)
}


#' convert a time series into a fractionally differentiated series
#'
#' @param x a vector of time series to be fractionally differentiated
#' @param d the order for fractionally differentiated features
#' @param nWei number of weights for output
#' @param tau threshold where weights are cut off; default is NULL, if not NULL
#'            then use tau and nWei is not used
#'            
#' @author Larry Lei Hua
#'
#' @export 
fracDiff <- function(x, d=0.5, nWei=10, tau=NULL)
{
  weig <- weights_fracDiff(d=d, nWei=nWei, tau=tau)
  nWei <- length(weig) # the first one in x that can use all the weights
  nx <- length(x)
  if(nWei > nx)
  {
    warning("Sample size is smaller than the number of weights, only use the weights up to the sample size.")
    nWei <- nx
  }
  rst <- rep(NA, nx)
  rst[nWei:nx] <- sapply(nWei:nx, function(i){ sum(weig[(1:nWei)] * x[i:(i-nWei+1)]) })
  return(rst)
}
