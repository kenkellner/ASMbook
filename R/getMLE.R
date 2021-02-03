#' Print Estimates and Standard Errors from optim Output
#'
#' @param opt Object resulting from a call to \code{optim}
#' @param dig Number of decimal places to use when printing
#'
#' @return Nothing; called for its side effects.
#'
#' @export
getMLE <- function(opt, dig = 3){
  MLE <- opt$par
  ASE <- rep(NA, length(MLE))
  if(!is.null(opt$hessian)){
    VC <- solve(opt$hessian)
    ASE <- sqrt(diag(VC))
  }
  print(cbind(MLE, ASE), dig)
}
