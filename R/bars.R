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
