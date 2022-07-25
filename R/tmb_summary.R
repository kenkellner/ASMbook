#' Summarize Output from TMB
#'
#' @param tmb_object A TMB object created by \code{MakeADFun} that has
#'  been optimized (e.g. with \code{optim})
#' @param dig Number of decimal places to use in output
#'
#' @return A matrix of parameter estimates and standard errors.
#'
#' @importFrom TMB sdreport summary.sdreport
#' @export
tmb_summary <- function(tmb_object, dig=NULL){
  npar <- length(tmb_object$par)
  pnames_fixed <- names(tmb_object$par)
  out <- summary(sdreport(tmb_object))
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
