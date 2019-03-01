#--------------------#
# LarryHua.com, 2019 #
#--------------------#

#' Construct time bars
#' 
#' @param dat dat input with at least the following columns: tStamp, Price, Size, where tStamp should be sorted already
#' @param tDur the time duration in seconds of each window
#' 
#' @return tStamp, seconds since the starting time point, and H,L,O,C,V
#' 
#' @export
bar_time <- function(dat, tDur=1)
{
  idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
  if(length(idx_time)>1) stop("More than one column with timestamp!")
  names(dat)[idx_time] <- "tStamp"
  t0 <- lubridate::floor_date(dat$tStamp[1])
  winIdx <- as.factor(floor((dat$tStamp - t0) / tDur))
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(as.double(dat$Size), by = list(winIdx), sum)$x
  tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
  list(tStamp=tStamp,sec=(as.integer(levels(winIdx))+1)*tDur,H=H,L=L,O=O,C=C,V=V)
}

#' Construct tick bars
#' 
#' @param dat dat input with at least the following columns: Price, Size
#' @param nTic the number of ticks of each window
#' 
#' @return time stamp at the end of each bar (if timestamp is provided), and H,L,O,C,V
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
  
  if(any(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t")))
  {
    idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
    names(dat)[idx_time] <- "tStamp"
    tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
    out <- list(tStamp=tStamp,H=H,L=L,O=O,C=C,V=V)
  }else{
    out <- list(H=H,L=L,O=O,C=C,V=V)
  }
  out
}

#' Construct volume bars
#' 
#' @param dat dat input with at least the following columns: Price, Size
#' @param vol the volume of each window
#' 
#' @return time stamp at the end of each bar (if timestamp is provided), and H,L,O,C,V
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
  
  if(any(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t")))
  {
    idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
    names(dat)[idx_time] <- "tStamp"
    tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
    out <- list(tStamp=tStamp,H=H,L=L,O=O,C=C,V=V)
  }else{
    out <- list(H=H,L=L,O=O,C=C,V=V)
  }
  out
}

#' Construct unit bars
#' 
#' @param dat dat input with at least the following columns: Price, Size. If timestamp is provided than output also contains timestamp of the unit bars
#' @param unit the total dollar (unit) traded of each window
#' 
#' @return time stamp at the end of each bar (if timestamp is provided), and H,L,O,C,V
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
  if(any(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t")))
  {
    idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
    names(dat)[idx_time] <- "tStamp"
    tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
    out <- list(tStamp=tStamp,H=H,L=L,O=O,C=C,V=V)
  }else{
    out <- list(H=H,L=L,O=O,C=C,V=V)
  }
  out
}

#' The auxiliary function b_t for constructing tick imbalance bars. The first b_t is assigned the value 0 because no information is available
#'
#' @param dat dat input with at least the following columns: Price
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(2), 0.5, 0.5, 0.4, runif(2) ))
#' 
#' b_t <- imbalance_tick(dat)
#' 
#' @export
imbalance_tick <- function(dat)
{
  # RLE encoding the prices means we don't have to deal with 0 price diffs.
  imbalance <- rle(dat$Price)
  imbalance$values <- sign(c(0, diff(imbalance$values)))
  # Invert the RLE to get the answer
  inverse.rle(imbalance)
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
    PminusP <- utils::tail(PminusP,1) # the last one is what we need
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
        E0T <- utils::tail(E0T,1)
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
#' @return a list of vectors for tStamp (if returned), and HLOCV of tick imbalance bars. Note that the remaining data after the latest imbalance time point will be formed as a bar.
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
  
  if(any(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t")))
  {
    idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
    names(dat)[idx_time] <- "tStamp"
    tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
    out <- list(tStamp=tStamp,H=H,L=L,O=O,C=C,V=V)
  }else{
    out <- list(H=H,L=L,O=O,C=C,V=V)
  }
  out
}


#' The auxiliary function b_tv_t for constructing volume imbalance bars. The first b_tv_t is assigned the value 0 because no information is available
#'
#' @param dat dat input with at least the following columns: Price, Size
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(10)), Size = rep(10,14))
#' 
#' b_tv_t <- imbalance_volume(dat)
#' 
#' @export
imbalance_volume <- function(dat)
{
  imbalance_tick(dat)*dat$Size  ## the main difference than imbalance_tick
}

#' Tstar index for Volume Imbalance Bars (bar_vib)
#' @param dat dat input with at least the following columns: Price, Size
#' @param w0 the time window length of the first bar
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_b backward window length when using pracma::movavg for exponentially weighted average b_tv_t
#' 
#' @return a vector for the lengths of the tick imbalance bars. For example, if the return is c(10,26), then the 2 tick imbalance bars are (0,10] and (10, 36]
#' 
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)), Size = rep(10, 54))
#' T_vib <- Tstar_vib(dat)
#' b_tv_t <- imbalance_volume(dat)
#' cumsum(b_tv_t)[cumsum(T_vib)] # check the accumulated b_t's where the imbalances occur
#' 
#' @export
Tstar_vib <- function(dat, w0=10, bkw_T=5, bkw_b=5)
{
  nx <- dim(dat)[1]
  b_tv_t <- imbalance_volume(dat) ## the main difference than imbalance_tick
  w0 <- max(min(which(cumsum(b_tv_t) != 0)), w0) # fix the case when there are always 0 at the beginning
  Tvec <- w0
  E0T <- Tvec
  repeat
  {
    T_last <- sum(Tvec) # the previous T that has been calculated
    nbt <- min(bkw_b, T_last - 1)
    PminusP <- pracma::movavg(b_tv_t[1:T_last], n=nbt, type="e") # the main difference than tib
    PminusP <- utils::tail(PminusP,1) # the last one is what we need
    b_tv_t_Expected <- E0T*abs(PminusP)
    b_tv_t_psum <- abs(cumsum(b_tv_t[-(1:T_last)]))
    
    # if max(b_tv_t_psum) < b_tv_t_Expected, then there is no chance of volume imbalance
    if(max(b_tv_t_psum) < b_tv_t_Expected){break}else
    {
      T_new <- min(which(b_tv_t_psum >= b_tv_t_Expected))
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
        E0T <- utils::tail(E0T,1)
      }
    }
  }
  return(Tvec)
}

#' Construct volume imbalance bars
#' 
#' @param dat dat input with at least the following column: Price, Size
#' @param w0 the time window length of the first bar
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_b backward window length when using pracma::movavg for exponentially weighted average b_tv_t
#' 
#' @return a list of vectors for tStamp (if returned), and HLOCV of volume imbalance bars. Note that the remaining data after the latest imbalance time point will be formed as a bar.
#'
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)), Size = rep(10,54))
#' bar_volume_imbalance(dat)
#' 
#' @export
bar_volume_imbalance <- function(dat, w0=10, bkw_T=5, bkw_b=5)
{
  T_vib <- Tstar_vib(dat, w0=w0, bkw_T=bkw_T, bkw_b=bkw_b)
  T_vib <- c(T_vib, nrow(dat)-sum(T_vib)) # the remaining data is treated as a bar
  
  winEnd <- cumsum(T_vib)
  winIdx <- as.factor(unlist(sapply(1:length(winEnd), function(i){rep(winEnd[i], T_vib[i])})))
  
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
  
  if(any(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t")))
  {
    idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
    names(dat)[idx_time] <- "tStamp"
    tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
    out <- list(tStamp=tStamp,H=H,L=L,O=O,C=C,V=V)
  }else{
    out <- list(H=H,L=L,O=O,C=C,V=V)
  }
  out
}

#' Construct tick runs bars
#' 
#' @param dat dat input with at least the following column: Price, Size
#' @param w0 the time window length of the first bar
#' @param de a positive value for adjusting the expected window size, that is, de*E0T; default: 1
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_Pb1 backward window length when using pracma::movavg for exponentially weighted average P[b_t=1]
#' @param filter whether used as a filter; default FALSE. If TRUE, then only i_feabar, the ending time index of feature bars, is returned
#' 
#' @return If filter==FALSE, a list of vectors for tStamp (if returned), and HLOCV of tick runs bars. Note that the remaining data after the latest ending time point detected will be formed as a bar.  If filter==TRUE, i_feabar a vector of integers for the time index.
#'
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(1000)), Size = rep(10,1004))
#' x1 <- bar_tick_runs(dat)
#' x2 <- bar_tick_runs(dat, filter=TRUE)
#' 
#' @export
bar_tick_runs <- function(dat, w0=10, de=1, bkw_T=5, bkw_Pb1=5, filter=FALSE)
{
  b_t <- imbalance_tick(dat)
  T_trb <- Tstar_trb_cpp(b_t, w0=w0, de=de, bkw_T=bkw_T, bkw_Pb1=bkw_Pb1)$Tstar
  
  if(filter==TRUE){
    out <- cumsum(T_trb)
  }else{
    T_trb <- c(T_trb, nrow(dat)-sum(T_trb)) # the remaining data is treated as a bar
    
    winEnd <- cumsum(T_trb)
    winIdx <- as.factor(unlist(sapply(1:length(winEnd), function(i){rep(winEnd[i], T_trb[i])})))
    
    H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
    L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
    O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
    C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
    V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
    
    if(any(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t")))
    {
      idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
      names(dat)[idx_time] <- "tStamp"
      tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
      out <- list(tStamp=tStamp,H=H,L=L,O=O,C=C,V=V)
    }else{
      out <- list(H=H,L=L,O=O,C=C,V=V)
    }
  }
  out
}

#' Construct volume runs bars
#' 
#' @param dat dat input with at least the following column: Price, Size
#' @param v_0 average volume for each trade, and it is used to create the first bar
#' @param w0 the time window length of the first bar
#' @param de a positive value for adjusting the expected window size, that is, de*E0T; default: 1
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_Pb1 backward window length when using pracma::movavg for exponentially weighted average P[b_t=1]
#' @param bkw_V backward window length for exponentially weighted average volumes
#' @param filter whether used as a filter; default FALSE. If TRUE, then only i_feabar, the ending time index of feature bars, is returned
#' 
#' @return If filter==FALSE, a list of vectors for tStamp (if returned), and HLOCV of volume runs bars. Note that the remaining data after the latest ending time point detected will be formed as a bar.  If filter==TRUE, i_feabar a vector of integers for the time index.
#'
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)), Size = floor(runif(54)*100))
#' bar_volume_runs(dat)
#' bar_volume_runs(dat, filter=TRUE)
#' 
#' @export
bar_volume_runs <- function(dat, v_0=20, w0=10, de=1, bkw_T=5, bkw_Pb1=5, bkw_V=5, filter=FALSE)
{
  b_t <- imbalance_tick(dat)
  v_t <- dat$Size
  T_vrb <- Tstar_vrb_cpp(b_t, v_t, v_0, w0, de, bkw_T, bkw_Pb1, bkw_V)$Tstar
  
  if(filter==TRUE){
    out <- cumsum(T_vrb)
  }else{
    T_vrb <- c(T_vrb, nrow(dat)-sum(T_vrb)) # the remaining data is treated as a bar  
    winEnd <- cumsum(T_vrb)
    winIdx <- as.factor(unlist(sapply(1:length(winEnd), function(i){rep(winEnd[i], T_vrb[i])})))
    
    H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
    L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
    O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
    C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
    V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
    
    if(any(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t")))
    {
      idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
      names(dat)[idx_time] <- "tStamp"
      tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
      out <- list(tStamp=tStamp,H=H,L=L,O=O,C=C,V=V)
    }else{
      out <- list(H=H,L=L,O=O,C=C,V=V)
    }
  }
  out
}

#' Construct unit runs bars
#' 
#' @param dat dat input with at least the following column: Price, Size
#' @param u_0 average unit (volume*price) for each trade, and it is used to create the first bar
#' @param w0 the time window length of the first bar
#' @param de a positive value for adjusting the expected window size, that is, de*E0T; default: 1
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_Pb1 backward window length when using pracma::movavg for exponentially weighted average P[b_t=1]
#' @param bkw_U backward window length for exponentially weighted average volumes
#' @param filter whether used as a filter; default FALSE. If TRUE, then only i_feabar, the ending time index of feature bars, is returned
#' 
#' @return If filter==FALSE, a list of vectors for tStamp (if returned), and HLOCV of volume runs bars. Note that the remaining data after the latest ending time point detected will be formed as a bar. If filter==TRUE, i_feabar a vector of integers for the time index.
#'
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)), Size = floor(runif(54)*100))
#' bar_unit_runs(dat, u_0=mean(dat$Price)*mean(dat$Size))
#' bar_unit_runs(dat, u_0=mean(dat$Price)*mean(dat$Size), filter=TRUE)
#' 
#' @export
bar_unit_runs <- function(dat, u_0=2000, w0=10, de=1, bkw_T=5, bkw_Pb1=5, bkw_U=5, filter=FALSE)
{
  b_t <- imbalance_tick(dat)
  v_t <- dat$Size
  p_t <- dat$Price
  T_urb <- Tstar_vrb_cpp(b_t, v_t*p_t, u_0, w0, de, bkw_T, bkw_Pb1, bkw_U)$Tstar
  
  if(filter==TRUE){
    out <- cumsum(T_urb)
  }else{
    T_urb <- c(T_urb, nrow(dat)-sum(T_urb)) # the remaining data is treated as a bar
    winEnd <- cumsum(T_urb)
    winIdx <- as.factor(unlist(sapply(1:length(winEnd), function(i){rep(winEnd[i], T_urb[i])})))
    
    H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
    L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
    O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
    C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
    V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
    
    if(any(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t")))
    {
      idx_time <- which(names(dat) %in% c("tStamp", "Timestamp", "timestamp", "time", "t"))
      names(dat)[idx_time] <- "tStamp"
      tStamp <- stats::aggregate(dat$tStamp, by = list(winIdx), function(x){x[length(x)]})$x
      out <- list(tStamp=tStamp,H=H,L=L,O=O,C=C,V=V)
    }else{
      out <- list(H=H,L=L,O=O,C=C,V=V)
    }
  }
  out
}
