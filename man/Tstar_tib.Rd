% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bars.R
\name{Tstar_tib}
\alias{Tstar_tib}
\title{Tstar index for Tick Imbalance Bars (bar_tib)}
\usage{
Tstar_tib(dat, w0 = 10, bkw_T = 5, bkw_b = 5)
}
\arguments{
\item{dat}{dat input with at least the following columns: Price}

\item{w0}{the expected time window length of the first bar}

\item{bkw_T}{backward window length when using exponentially weighted average T}

\item{bkw_b}{backward window length when using exponentially weighted average b_t}
}
\value{
a vector for the lengths of the tick imbalance bars. For example, if the return is c(33,711), then the 2 tick imbalance bars are (0,33] and (33, 744]
}
\description{
Tstar index for Tick Imbalance Bars (bar_tib)
}
\examples{

set.seed(1)
dat <- data.frame(Price = c(rep(0.5, 4), runif(1000)))
T_tib <- Tstar_tib(dat)
b_t <- imbalance_tick(dat)
cumsum(b_t)[cumsum(T_tib)] # check the accumulated b_t's where the imbalances occur

}
\author{
Larry Lei Hua
}
