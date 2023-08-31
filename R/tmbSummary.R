#' Summarize Output from TMB
#'
#' @param tmbObject A TMB object created by \code{MakeADFun} that has
#'  been optimized (e.g. with \code{optim})
#' @param dig Number of decimal places to use in output
#'
#' @return A matrix of parameter estimates and standard errors.
#'
#' @author Ken Kellner
#'
#' @importFrom TMB sdreport summary.sdreport
#' @export
tmbSummary <- function(tmbObject, dig=NULL){
  npar <- length(tmbObject$par)
  pnames_fixed <- names(tmbObject$par)
  out <- summary(sdreport(tmbObject))
  pnames <- rownames(out)
  pcount <- sapply(unique(pnames), function(x) sum(pnames==x))
  idx <- unlist(sapply(pcount, function(i){
    if(i == 1) return("")
    paste0("[",1:i,"]")
    }))
  pnames <- paste0(pnames, idx)
  pnames[1:npar] <- pnames_fixed
  rownames(out) <- pnames
  z <- stats::qnorm(0.025, lower.tail=FALSE)
  lower <- out[,"Estimate"] - z*out[,"Std. Error"]
  upper <- out[,"Estimate"] + z*out[,"Std. Error"]
  out <- cbind(out, LCL.95=lower, UCL.95=upper)
  if(!is.null(dig)) out <- round(out, dig)
  out
}

#' @rdname tmbSummary
#' @export
tmb_summary <- tmbSummary
