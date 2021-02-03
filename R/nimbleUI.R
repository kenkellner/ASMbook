#S3Class("nimbleUI")

#' Fit a Model with Nimble
#'
#' This function is a wrapper for fitting a model using Nimble, potentially
#' running MCMC chains run in parallel. See ?nimble::nimbleMCMC for more details.
#'
#' @param code Code expression representing model, output from \code{nimbleCode}
#' @param constants Named list of constants in the model
#' @param inits A list of initial values or function to generate such a list
#' @param monitors A character vector of parameters in the model to monitor
#' @param thin Thinning interval for MCMC
#' @param niter Number of MCMC iterations to run
#' @param nburnin Number of iterations to discard as burn-in
#' @param nchains Number of MCMC chains to run
#' @param parallel If \code{TRUE}, run MCMC chains in parallel
#' @param ncores Manually set number of cores to use when running in parallel.
#'  If \code{NULL} (the default), the number of cores will be the number of chains
#'  or the number of available cores - 1, whichever is smaller.

#' @return An object of class \code{nimbleUI} containing MCMC samples
#'  as an \code{mcmc.list}, and other info.
#'
#' @seealso \code{\link{nimbleMCMC}}
#'
#' @importFrom nimble nimbleMCMC
#' @importFrom parallel detectCores makeCluster stopCluster clusterCall
#' @importFrom parallel parLapply
#' @importFrom stats sd quantile na.omit
#' @importFrom coda as.mcmc.list
#' @export
nimbleUI <- function(code, constants, inits, monitors,
                     thin=1, niter=10000, nburnin=0, nchains=1,
                     parallel=FALSE, ncores=NULL){

  if(parallel){
    input_list <- list(code=code, constants=constants, inits=inits, monitors=monitors,
                   n_burnin=nburnin, n_iter=niter, n_thin=thin, n_chains=nchains)

    avail_cores <- parallel::detectCores()
    ncores <- ifelse(is.null(ncores), min(avail_cores-1, nchains), ncores)
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))

    catch <- parallel::clusterCall(cl, function() attachNamespace("nimble"))

    cat("Running chains in parallel\n")
    mcmc <- suppressWarnings(parallel::parLapply(cl, X=1:nchains, function(x, inputs){

      nimbleMCMC(code=inputs$code, constants=inputs$constants, inits=inputs$inits,
                 monitors=inputs$monitors,nburnin=inputs$n_burnin, niter=inputs$n_iter,
                 thin=inputs$n_thin, nchains=1, samplesAsCodaMCMC=TRUE)

    }, inputs=input_list))
    samples <- coda::as.mcmc.list(mcmc)
  } else {
    samples <- nimbleMCMC(code=code, constants=constants, inits=inits, monitors=monitors,
                          nburnin=nburnin, niter=niter, thin=thin, nchains=nchains,
                          samplesAsCodaMCMC=TRUE)
  }

  out <- list(samples=samples, params=monitors)
  class(out) <- "nimbleUI"
  out
}

#' Summarize MCMC Samples in nimbleUI Object
#'
#' @param object An object of class \code{nimbleUI}
#' @param ... Currently ignored
#'
#' @return A data frame of summary information for each saved parameter
#'
#' @rdname summary.nimbleUI
#' @export
summary.nimbleUI <- function(object, ...){
  samples <- order_params(object$samples, object$params)
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

#' Print Summary of nimbleUI Object to the Console
#'
#' @param x An object of class \code{nimbleUI}
#' @param digits Number of digits to use when rounding
#' @param ... Other arguments passed to \code{print}
#'
#' @return Doesn't return anything; called for its side effects.
#'
#' @rdname print.nimbleUI
#' @export
print.nimbleUI <- function(x, digits=3, ...){
  nchain <- length(x$samples)
  niter <- nrow(x$samples[[1]])
  out <- summary(x)
  cat("Estimates based on",nchain,"chains of",niter,"iterations\n")
  print(round(out, digits=digits))
}


