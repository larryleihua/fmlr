#' @useDynLib fmlr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("fmlr", libpath)
}

