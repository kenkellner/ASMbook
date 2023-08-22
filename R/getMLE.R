#' Print Estimates and Standard Errors from optim Output
#'
#' @param opt Object resulting from a call to \code{optim}
#' @param dig Number of decimal places to use when printing
#'
#' @return A matrix of parameter estimates, standard errors, and 95% CI.
#'
#' @name getMLE
#' @export
getMLE <- function(opt, dig = 3){
  MLE <- opt$par
  ASE <- lower <- upper <- rep(NA, length(MLE))
  if(!is.null(opt$hessian)){
    VC <- solve(opt$hessian)
    ASE <- sqrt(diag(VC))
    z <- qnorm(0.025, lower.tail=FALSE)
    lower <- MLE - z*ASE
    upper <- MLE + z*ASE
  }
  out <- cbind(MLE, ASE, LCL.95=lower, UCL.95=upper)
  print(out, dig)
  out
}

#' @rdname getMLE
#' @export
get_MLE <- getMLE