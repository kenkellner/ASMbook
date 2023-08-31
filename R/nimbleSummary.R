#' Summarize MCMC Samples in an mcmc.list Object Created by NIMBLE
#'
#' @param samples An object of class \code{mcmc.list}
#' @param params An optional list of the parameter names used to sort the output
#'
#' @return A data frame of summary information for each saved parameter
#'
#' @author Ken Kellner
#' 
#' @export
nimbleSummary <- function(samples, params=NULL){
  if(!is.null(params)) samples <- order_params(samples, params)
  mat <- as.matrix(samples)
  rhat <- sapply(1:ncol(samples[[1]]), function(i){
    coda::gelman.diag(samples[,i], autoburnin=FALSE)$psrf[1,1]
  })
  stats <- t(apply(mat, 2, function(x){
    x <- stats::na.omit(x)
    c(mean=mean(x), sd=stats::sd(x), stats::quantile(x, c(0.025,0.5,0.975)))
  }))
  data.frame(stats, Rhat=rhat, check.names=FALSE)
}

order_params <- function(samples, parameters.to.save){

  params <- colnames(samples[[1]])
  params <- params[order(match(sapply(strsplit(params, "\\["), "[", 1),
                               sapply(strsplit(parameters.to.save, "\\["), "[", 1)))]

  samples <- samples[,params, drop=FALSE]

  return(samples)

}

#' @rdname nimbleSummary
#' @export
nimble_summary <- nimbleSummary
