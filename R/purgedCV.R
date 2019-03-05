#--------------------#
# LarryHua.com, 2019 #
#--------------------#

#' Purged k-fold CV with embargo
#' @param feaMat a data.frame for feature matrix with the first column being the label
#                "t1Fea": time index for features bars, i.e., the time index at the end of each features bars
#                "tLabel": time index where events occur (e.g., touching some barrier)
#' @param k number of folds for k-fold CV
#' @param gam gamma for embargo
#' @return a list of k data.frame, each containing a test set and a training set
#' @examples
#' feaMat <- data.frame(Y = c(1,1,0,1,0),
#'                      V = c(2,4,2,4,1), 
#'                      t1Fea = c(2,5,8,14,20), 
#'                      tLabel = c(4,12,16,23,38))
#' purged_k_CV(feaMat, k=2, gam=0.1)
#' 
#' @author Larry Lei Hua
#' 
#' @export
purged_k_CV <- function(feaMat,k=5,gam=0.01)
{
  I <- nrow(feaMat)
  nFold <- c(rep(wd <- floor(I/k), k-1), I - wd*(k-1)) # size of each fold
  cumnFold <- c(0,cumsum(nFold))
  out <- lapply(1:k, function(i){
    testInx <- (cumnFold[i]+1):cumnFold[i+1]
    testFold <- feaMat[testInx,]
    trainFold <- feaMat[setdiff(1:I,testInx),]

    # purge with embargo
    TT <- max(feaMat$tLabel)
    h <- gam*TT
    tEmbargo <- max(testFold$tLabel)+h # last time index of the test set + embargo
    t0Test <- min(testFold$t1Fea)+1 # first time index of the test set, 
                                    # +1 because the info at time 0 of the feature bar is not used for labeling
    trainFold <- subset( trainFold, (tLabel<t0Test) | (t1Fea>=tEmbargo) )
    list(testSet=testFold, trainSet=trainFold)
  })
  return(out)
}
