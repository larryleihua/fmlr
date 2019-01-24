#--------------------#
# LarryHua.com, 2019 #
#--------------------#

#' Construct time bars
#' 
#' @param dat dat input with at least the following columns: tStamp, Price, Size
#' @param tDur the time duration in seconds of each window
#' 
#' @export
bar_time <- function(dat, tDur=1)
{
  t0 <- lubridate::floor_date(min(dat$tStamp))
  winIdx <- as.factor(floor((dat$tStamp - t0) / tDur))
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(as.double(dat$Size), by = list(winIdx), sum)$x
  list(H=H,L=L,O=O,C=C,V=V)
}

#' Construct tick bars
#' 
#' @param dat dat input with at least the following columns: Price, Size
#' @param nTic the number of ticks of each window
#' 
#' @export
bar_tick <- function(dat, nTic)
{
  n <- dim(dat)[1]
  winIdx <- as.factor(floor((1:n)/nTic))
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
  list(H=H,L=L,O=O,C=C,V=V)
}

#' Construct volume bars
#' 
#' @param dat dat input with at least the following columns: Price, Size
#' @param vol the volume of each window
#' 
#' @export
bar_volume <- function(dat, vol)
{
  cumVol <- cumsum(dat$Size)
  winIdx <- as.factor(floor(cumVol / vol))
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
  list(H=H,L=L,O=O,C=C,V=V)
}

#' Construct unit bars
#' 
#' @param dat dat input with at least the following columns: Price, Size
#' @param unit the total dollar (unit) traded of each window
#' 
#' @export
bar_unit <- function(dat, unit)
{
  cumUnit <- cumsum(dat$Size*dat$Price)
  winIdx <- as.factor(floor(cumUnit / unit))
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
  list(H=H,L=L,O=O,C=C,V=V)
}

#' The auxiliary function b_t for constructing tick imbalance bars. The first b_t is assigned the value 0 because no information is available
#'
#' @param dat dat input with at least the following columns: Price
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(10)))
#' 
#' b_t <- imbalance_tick(dat)
#' 
#' @export
imbalance_tick <- function(dat)
{
  n <- length(dat$Price)
  imbalance <- rep(0, n)
  price_diff <- diff(dat$Price)
  for(i in 2:n)
  {
    imbalance[i] <- sign(price_diff[i-1])*(price_diff[i-1]!=0) + imbalance[i-1]*(price_diff[i-1]==0)
  }
  imbalance
}

#' Tstar index for Tick Imbalance Bars (bar_tib)
#' @param dat dat input with at least the following columns: Price
#' @param w0 the time window length of the first bar
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_b backward window length when using pracma::movavg for exponentially weighted average b_t
#' 
#' @return a vector for the lengths of the tick imbalance bars. For example, if the return is c(10,26), then the 2 tick imbalance bars are (0,10] and (10, 36]
#' 
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)))
#' T_tib <- Tstar_tib(dat)
#' b_t <- imbalance_tick(dat)
#' cumsum(b_t)[cumsum(T_tib)] # check the accumulated b_t's where the imbalances occur
#' 
#' @export
Tstar_tib <- function(dat, w0=10, bkw_T=5, bkw_b=5)
{
  nx <- dim(dat)[1]
  b_t <- imbalance_tick(dat)
  w0 <- max(min(which(cumsum(b_t) != 0)), w0) # fix the case when there are always 0 at the beginning
  Tvec <- w0
  E0T <- Tvec
  repeat
  {
    T_last <- sum(Tvec) # the previous T that has been calculated
    nbt <- min(bkw_b, T_last - 1)
    PminusP <- pracma::movavg(b_t[1:T_last], n=nbt, type="e")
    PminusP <- tail(PminusP,1) # the last one is what we need
    b_t_Expected <- E0T*abs(PminusP)
    b_t_psum <- abs(cumsum(b_t[-(1:T_last)]))
    
    # if max(b_t_psum) < b_t_Expected, then there is no chance of tick imbalance
    if(max(b_t_psum) < b_t_Expected){break}else
    {
      T_new <- min(which(b_t_psum >= b_t_Expected))
      # cat(T_last, PminusP, T_new, "\n")
    }
    T_last <- T_last + T_new
    if(T_last > nx){break}else
    {
      Tvec <- c(Tvec, T_new)
      nTvec <- length(Tvec)
      if(nTvec <= 2)
      {
        E0T <- mean(Tvec) # not enough T for exponential weighted average, so use the mean
      }else
      {
        nT <- min(bkw_T, length(Tvec)-1)
        E0T <- pracma::movavg(Tvec[1:nTvec], n=nT, type = "e")
        E0T <- tail(E0T,1)
      }
    }
  }
  return(Tvec)
}

#' Construct tick imbalance bars
#' 
#' @param dat dat input with at least the following column: Price, Size
#' @param w0 the time window length of the first bar
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_b backward window length when using pracma::movavg for exponentially weighted average b_t
#' 
#' @return a list of vectors for HLOCV of tick imbalance bars. Note that the remaining data after the latest imbalance time point will be formed as a bar.
#'
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)), Size = rep(10,54))
#' bar_tick_imbalance(dat)
#' 
#' @export
bar_tick_imbalance <- function(dat, w0=10, bkw_T=5, bkw_b=5)
{
  T_tib <- Tstar_tib(dat, w0=w0, bkw_T=bkw_T, bkw_b=bkw_b)
  T_tib <- c(T_tib, nrow(dat)-sum(T_tib)) # the remaining data is treated as a bar
  
  winEnd <- cumsum(T_tib)
  winIdx <- as.factor(unlist(sapply(1:length(winEnd), function(i){rep(winEnd[i], T_tib[i])})))
  
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
  list(H=H,L=L,O=O,C=C,V=V)
}

