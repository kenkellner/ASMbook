#' Summarize Output from TMB
#'
#' @param tmbObj A TMB object created by \code{MakeADFun} that has
#'  been optimized (e.g. with \code{optim})
#' @param dig Number of decimal places to use in output
#'
#' @return A matrix of parameter estimates and standard errors.
#'
#' @importFrom TMB sdreport summary.sdreport
#' @export
tmbSummary <- function(tmbObj, dig=NULL){
  npar <- length(tmbObj$par)
  pnames_fixed <- names(tmbObj$par)
  out <- summary(sdreport(tmbObj))
  pnames <- rownames(out)
  pcount <- sapply(unique(pnames), function(x) sum(pnames==x))
  idx <- unlist(sapply(pcount, function(i){
    if(i == 1) return("")
    paste0("[",1:i,"]")
    }))
  pnames <- paste0(pnames, idx)
  pnames[1:npar] <- pnames_fixed
  rownames(out) <- pnames
  if(!is.null(dig)) out <- round(out, dig)
  out
}
