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
  n <- length(dat$Price)
  vol <- dat$Size  ## the main difference than imbalance_tick
  imbalance <- rep(0, n)
  price_diff <- diff(dat$Price)
  for(i in 2:n)
  {
    imbalance[i] <- sign(price_diff[i-1])*(price_diff[i-1]!=0)*vol[i] + imbalance[i-1]*(price_diff[i-1]==0)
  }
  imbalance
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
    PminusP <- tail(PminusP,1) # the last one is what we need
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
        E0T <- tail(E0T,1)
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
#' @return a list of vectors for HLOCV of volume imbalance bars. Note that the remaining data after the latest imbalance time point will be formed as a bar.
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
  list(H=H,L=L,O=O,C=C,V=V)
}



#' Tstar index for Tick Runs Bars (bar_trb)
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
#' T_trb <- Tstar_trb(dat)
#' b_t <- imbalance_tick(dat)
#' cumsum(b_t)[cumsum(T_trb)] # check the accumulated b_t's where the imbalances occur
#' 
#' @export
Tstar_trb <- function(dat, w0=10, bkw_T=5, bkw_Pb1=5)
{
  b_t <- imbalance_tick(dat)
  nb <- length(b_t)
  nx <- dim(dat)[1]
  
  # calculate the length of the 1st run
  th_T <- sapply(1:nb, function(i){
    b_t_tmp <- b_t[1:i]
    if(sum(b_t_tmp %in% c(-1,1))==0){out <- 0}else
    {
      out <- max(sum(b_t_tmp[b_t_tmp==1]), -sum(b_t_tmp[b_t_tmp==-1]))  
    }
    out
  })
  
  w0 <- max(min(which(th_T != 0)), w0) # fix the case when there are always 0 at the beginning
  w0 <- max(min(which(b_t==1)), w0) # there must be at least 1 b_t = 1 during the first window
  Tvec <- w0
  E0T <- T_last <- Tvec
  Pb1 <- sum(b_t[1:w0]==1) / w0 # Pb1: Pr[b_t = 1]
  Pb1vec <- Pb1
  th_T_Expected <- E0T*max(Pb1, 1-Pb1)
  while(T_last<nx-1)
  {
    T_last <- sum(Tvec) # the last T that has been calculated
    for(j in 1:(nb-T_last-1))
    {
      b_t_tmp <- b_t[(T_last+1):(T_last+j)]
      if(sum(b_t_tmp %in% c(-1,1))==0){th_T_tmp <- 0}else
      {
        th_T_tmp <- max(sum(b_t_tmp[b_t_tmp==1]), -sum(b_t_tmp[b_t_tmp==-1]))
      }
      if(th_T_tmp >= th_T_Expected)
      {
        new_flag <- TRUE # new window generated!
        T_new <- j
        Tvec <- c(Tvec, T_new)
        T_last <- T_last + T_new
        Pb1_new <- sum(b_t_tmp==1) / j
        Pb1vec <- c(Pb1vec, Pb1_new)
        break
      }
    }
    
    if(new_flag==TRUE)
    {
      new_flag <- FALSE
      nTvec <- length(Tvec) # nTvec should be the same as nPb1vec
      if(nTvec <= 2)
      {
        E0T <- mean(Tvec) # not enough T for exponential weighted average, so use the mean
        Pb1 <- mean(Pb1vec)
      }else
      {
        nT <- min(bkw_T, length(Tvec)-1) 
        E0T <- pracma::movavg(Tvec[1:nTvec], n=nT, type = "e")
        E0T <- E0T[length(E0T)]
        nPb1 <- min(bkw_Pb1, length(Tvec)-1)
        Pb1 <- pracma::movavg(Pb1vec[1:nTvec], n=nPb1, type = "e")
        Pb1 <- Pb1[length(Pb1)]
      }
      th_T_Expected <- E0T*max(Pb1, 1-Pb1)
    }else{break}
  }
  return(Tvec)
}


#' Construct tick runs bars
#' 
#' @param dat dat input with at least the following column: Price, Size
#' @param w0 the time window length of the first bar
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_Pb1: backward window length when using pracma::movavg for exponentially weighted average P[b_t=1]
#' 
#' @return a list of vectors for HLOCV of tick runs bars. Note that the remaining data after the latest ending time point detected will be formed as a bar.
#'
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)), Size = rep(10,54))
#' bar_tick_runs(dat)
#' 
#' @export
bar_tick_runs <- function(dat, w0=10, bkw_T=5, bkw_Pb1=5)
{
  T_trb <- Tstar_trb(dat, w0=w0, bkw_T=bkw_T, bkw_Pb1=bkw_Pb1)
  T_trb <- c(T_trb, nrow(dat)-sum(T_trb)) # the remaining data is treated as a bar
  
  winEnd <- cumsum(T_trb)
  winIdx <- as.factor(unlist(sapply(1:length(winEnd), function(i){rep(winEnd[i], T_trb[i])})))
  
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
  list(H=H,L=L,O=O,C=C,V=V)
}

#' Tstar index for Volume Runs Bars (bar_vrb)
#' @param dat dat input with at least the following columns: Price, Size
#' @param w0 the time window length of the first bar
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_Pb1: backward window length when using pracma::movavg for exponentially weighted average P[b_t=1]
#' 
#' @return a vector for the lengths of the tick runs bars. For example, if the return is c(10,26), then the 2 tick runs bars are (0,10] and (10, 36]
#' 
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)),
#'                   Size = rep(10,54))
#' T_vrb <- Tstar_vrb(dat)
#' 
#' @export
Tstar_vrb <- function(dat, w0=10, bkw_T=5, bkw_Pb1=5)
{
  b_t <- imbalance_tick(dat)
  nb <- length(b_t)
  nx <- dim(dat)[1]
  vol <- dat$Size[1:nb]
  
  # calculate the length of the 1st run
  th_T <- sapply(1:nb, function(i){
    b_t_tmp <- b_t[1:i]
    vol_t_tmp <- vol[1:i]
    if(sum(b_t_tmp %in% c(-1,1))==0){out <- 0}else
    {
      out <- max( sum( b_t_tmp[b_t_tmp==1]*vol_t_tmp[b_t_tmp==1] ), -sum( b_t_tmp[b_t_tmp==-1]*vol_t_tmp[b_t_tmp==-1] ) )  
    }
    out
  })
  
  w0 <- max(min(which(th_T != 0)), w0) # fix the case when there are always 0 at the beginning
  w0 <- max(min(which(b_t==1)), w0) # there must be at least 1 b_t = 1 during the first window
  Tvec <- w0
  E0T <- T_last <- Tvec
  Pb1 <- sum(b_t[1:w0]==1) / w0 # Pb1: Pr[b_t = 1]
  Pb1vec <- Pb1
  
  # the main difference than the tick runs bar
  vol_tmp <- vol[1:w0]
  volvec_up <- vol_tmp_up <- mean(vol_tmp[b_t[1:w0] == 1]) # the first expected v_t|b_t=1 
  volvec_down <- vol_tmp_down <- mean(vol_tmp[b_t[1:w0] == -1]) # the first expected v_t|b_t=-1
  th_T_Expected <- E0T*max(Pb1*vol_tmp_up, (1-Pb1)*vol_tmp_down)
  
  while(T_last<nx-1)
  {
    T_last <- sum(Tvec) # the last T that has been calculated
    for(j in 1:(nb-T_last-1))
    {
      b_t_tmp <- b_t[(T_last+1):(T_last+j)]
      vol_tmp <- vol[(T_last+1):(T_last+j)]
      if(sum(b_t_tmp %in% c(-1,1))==0){th_T_tmp <- 0}else
      {
        th_T_tmp <- max( sum( b_t_tmp[b_t_tmp==1]*vol_tmp[b_t_tmp==1] ), -sum( b_t_tmp[b_t_tmp==-1]*vol_tmp[b_t_tmp==-1] ) )  
      }
      if(th_T_tmp >= th_T_Expected)
      {
        new_flag <- TRUE # new window generated!
        T_new <- j
        Tvec <- c(Tvec, T_new)
        T_last <- T_last + T_new
        
        Pb1_new <- sum(b_t_tmp==1) / j
        volup_new <- ifelse(sum(b_t_tmp == 1)>0, mean(vol_tmp[b_t_tmp == 1]),0)
        voldown_new <- ifelse(sum(b_t_tmp == -1)>0, mean(vol_tmp[b_t_tmp == -1]),0)
        
        Pb1vec <- c(Pb1vec, Pb1_new)
        volvec_up <- c(volvec_up, volup_new)
        volvec_down <- c(volvec_down, voldown_new)
        
        break # whenever new window is generated, we should stop the for loop and then deal with the new window
      }
    }
    
    if(new_flag==TRUE)
    {
      new_flag <- FALSE
      nTvec <- length(Tvec) # nTvec should be the same as nPb1vec
      if(nTvec <= 2)
      {
        E0T <- mean(Tvec) # not enough T for exponential weighted average, so use the mean
        Pb1 <- mean(Pb1vec)
        E0V_up <- mean(volvec_up)
        E0V_down <- mean(volvec_down)  
      }else
      {
        nT <- min(bkw_T, length(Tvec)-1) 
        E0T <- pracma::movavg(Tvec[1:nTvec], n=nT, type = "e")
        E0T <- E0T[length(E0T)] # equivalent to tail(E0T,1)
        nPb1 <- min(bkw_Pb1, length(Tvec)-1)
        Pb1 <- pracma::movavg(Pb1vec[1:nTvec], n=nPb1, type = "e")
        Pb1 <- Pb1[length(Pb1)]
        E0V_up <- pracma::movavg(volvec_up[1:nTvec], n=nPb1, type = "e") # use the same nPb1 for E0V_up
        E0V_up <- E0V_up[length(E0V_up)]
        E0V_down <- pracma::movavg(volvec_down[1:nTvec], n=nPb1, type = "e") # use the same nPb1 for E0V_down
        E0V_down <- E0V_down[length(E0V_down)]
      }
      th_T_Expected <- E0T*max(Pb1*E0V_up, (1-Pb1)*E0V_down)
    }else{break}
  }
  return(Tvec)
}

#' Construct volume runs bars
#' 
#' @param dat dat input with at least the following column: Price, Size
#' @param w0 the time window length of the first bar
#' @param bkw_T backward window length when using pracma::movavg for exponentially weighted average T
#' @param bkw_Pb1: backward window length when using pracma::movavg for exponentially weighted average P[b_t=1]
#' 
#' @return a list of vectors for HLOCV of tick runs bars. Note that the remaining data after the latest ending time point detected will be formed as a bar.
#'
#' @examples
#' 
#' set.seed(1)
#' dat <- data.frame(Price = c(rep(0.5, 4), runif(50)), Size = floor(runif(54)*100))
#' bar_volume_runs(dat)
#' 
#' @export
bar_volume_runs <- function(dat, w0=10, bkw_T=5, bkw_Pb1=5)
{
  T_vrb <- Tstar_vrb(dat, w0=w0, bkw_T=bkw_T, bkw_Pb1=bkw_Pb1)
  T_vrb <- c(T_vrb, nrow(dat)-sum(T_vrb)) # the remaining data is treated as a bar
  
  winEnd <- cumsum(T_vrb)
  winIdx <- as.factor(unlist(sapply(1:length(winEnd), function(i){rep(winEnd[i], T_vrb[i])})))
  
  H <- stats::aggregate(dat$Price, by = list(winIdx), max)$x
  L <- stats::aggregate(dat$Price, by = list(winIdx), min)$x
  O <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[1]})$x
  C <- stats::aggregate(dat$Price, by = list(winIdx), function(x){x[length(x)]})$x
  V <- stats::aggregate(dat$Size, by = list(winIdx), sum)$x
  list(H=H,L=L,O=O,C=C,V=V)
}



