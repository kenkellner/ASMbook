#' Fit a Poisson GLM with MCMC
#'
#' This is a demo function that fits a Poisson GLM with one continuous covariate
#' to some data (y, x) using a random-walk Metropolis Markov chain Monte Carlo algorithm.
#'
#' @param y A vector of counts, e.g., y in the Swiss bee-eater example
#' @param x A vector of a continuous explanatory variable, e.g. year x in the bee-eaters
#' @param true.vals True intercept and slope if known (i.e., when run on simulated data)
#' @param inits Initial values in the MCMC algorithm for alpha, beta
#' @param prior.sd.alpha SD of normal prior for alpha
#' @param prior.sd.beta SD of normal prior for beta
#' @param tuning.params SD of the Gaussian proposal distributions for alpha, beta
#' @param niter Total chain length (before burnin)
#' @param nburn Burn-in length
#' @param quiet Logical, suppress console output
#' @param show.plots Logical, should diagnostic plots be shown?
#'
#' @return A list containing input settings, acceptance probabilities and MCMC samples.
#'
#' @author Marc Kéry
#'
#' @examples
#' # Load the real data used in the publication by Mueller (Vogelwarte, 2021)
#'
#' # Counts of known pairs in the country 1990-2020
#' y <- c(0,2,7,5,1,2,4,8,10,11,16,11,10,13,19,31,
#'       20,26,19,21,34,35,43,53,66,61,72,120,102,159,199)
#' year <- 1990:2020     # Define year range
#' x <- (year-1989)      # Scaled, but not centered year as a covariate
#' x <- x-16             # Now it's centered
#'
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow = c(1, 2), mar = c(5,5,5,2), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
#' plot(table(y), xlab = 'Count (y)', ylab = 'Frequency', frame = FALSE,
#'      type = 'h', lend = 'butt', lwd = 5, col = 'gray20', main = 'Frequency distribution of counts')
#' plot(year, y, xlab = 'Year (x)', ylab = 'Count (y)', frame = FALSE, cex = 1.5,
#'      pch = 16,  col = 'gray20', main = 'Relationship y ~ x')
#' fm <- glm(y ~ x, family = 'poisson')        # Add Poisson GLM line of best fit
#' lines(year, predict(fm, type = 'response'), lwd = 3, col = 'red', lty = 3)
#'
#' \donttest{
#' # Execute the function with default function args
#' # In a real test you should run more iterations
#' par(mfrow = c(1,1))
#' str(tmp <- demoMCMC(niter=100, nburn=50))
#' 
#' # Use data created above
#' par(mfrow = c(1,1))
#' str(tmp <- demoMCMC(y = y, x = x, niter=100, nburn=50))
#'
#' }
#' par(oldpar)
#'
#' @importFrom stats dpois dnorm rnorm runif sd quantile
#' @importFrom graphics abline points
#' @export
demoMCMC <- function(y, x,
  true.vals = c(2.5, 0.14),
  inits = c(0, 0),
  prior.sd.alpha = 100, prior.sd.beta = 100,
  tuning.params = c(0.1, 0.1),
  niter = 10000, nburn = 1000,
  quiet = TRUE, show.plots = TRUE){
  #
  # This is a demo function that fits a Poisson GLM with one continuous covariate
  # to some data (y, x) using a random-walk Metropolis algorithm.
  # Both parameters are estimated on the log link scale and 
  # we give Gaussian priors with SD as chosen in the function args.
  stopifnot(nburn < niter)

  start.time <- Sys.time()

  # Create x and y if they aren't provided
  if(missing(y)){
    y <- c(0,2,7,5,1,2,4,8,10,11,16,11,10,13,19,31,
           20,26,19,21,34,35,43,53,66,61,72,120,102,159,199)
  }
  if(missing(x)){
    year <- 1990:2020     # Define year range
    x <- (year-1989)      # Scaled, but not centered year as a covariate
    x <- x-16             # Now it's centered
  }
  stopifnot(length(x) == length(y))
  
  # Create an R object to hold the random posterior draws produced
  out <- matrix(NA, niter, 2, dimnames = list(NULL, c("alpha", "beta")))

  # R object to hold acceptance indicator for both params
  acc <- matrix(0, niter, 2, dimnames = list(NULL, c("alpha", "beta")))
  
  # Initialize parameters: these are the initial values
  alpha <- inits[1]
  beta <- inits[2]

  # Numerical save setting to avoid lambda getting equal to 0.
  precision <- 700

  # Avoid log-lambda from getting equal to 0
  loglam.curr <- alpha + beta*x
  loglam.curr[loglam.curr < -precision] -precision  
  logpost.curr <-
    sum(dpois(y, exp(loglam.curr), log=TRUE)) +      # log-likelihood ...
    dnorm(alpha, 0, prior.sd.alpha, log=TRUE) + # ... plus logPrior(mu) ...
    dnorm(beta, 0, prior.sd.beta, log=TRUE)     # logPrior(log.sigma)

  # Run MCMC algorithm
  for(i in 1:niter){
    if(i %% 250 == 0) # report progress
    message("iter ", i)

    ### First, update log-linear intercept (alpha)
    # Propose candidate value of alpha
    alpha.cand <- rnorm(1, alpha, tuning.params[1])
    # Evaluate the log(posterior) for proposed new alpha and current beta for data y with cov x
    loglam.cand <- alpha.cand + beta*x
    loglam.cand[loglam.cand < -precision] -precision  
    logpost.cand <-
      sum(dpois(y, exp(loglam.cand), log=TRUE)) +      # log-likelihood ...
      dnorm(alpha.cand, 0, prior.sd.alpha, log=TRUE) + # ... plus logPrior(alpha.cand) ...
      dnorm(beta, 0, prior.sd.beta, log=TRUE)          # logPrior(beta)

	# Compute Metropolis acceptance probability, r, for alpha update
    r <- exp(logpost.cand - logpost.curr)
    # Keep candidate if it meets criterion (u < r)
    if(runif(1) < r){
      alpha <- alpha.cand
      logpost.curr <- logpost.cand
      acc[i,1] <- 1           # Indicator for whether candidate alpha accepted
 	}
 
    # Second, update log-linear slope (beta)
    # -------------------------------------
    beta.cand <- rnorm(1, beta, tuning.params[2]) # note again the tuning 'parameter'

    # Evaluate the log(posterior) for proposed new beta and for the current alpha for data y with cov x 
    loglam.cand <- alpha + beta.cand*x
    loglam.cand[loglam.cand < -precision] -precision  
    logpost.cand <-
      sum(dpois(y, exp(loglam.cand), log=TRUE)) +  # log-likelihood ...
      dnorm(alpha, 0, prior.sd.alpha, log=TRUE) +  # ... plus logPrior(alpha) ...
      dnorm(beta.cand, 0, prior.sd.beta, log=TRUE) # logPrior(beta.cand)

    # Compute Metropolis acceptance probability, r, for beta update
    r <- exp(logpost.cand - logpost.curr)
    # Keep candidate if it meets criterion (u < r)
    if(runif(1) < r){
      beta <- beta.cand
      logpost.curr <- logpost.cand
      acc[i,2] <- 1           # Indicator for whether candidate alpha accepted
	}
	out[i,] <- c(alpha, beta) # Save pair of draws for iteration i

    # Dynamic plots, can add something like this
    # ----------------------------------------------
    plot(out[1:i,1], out[1:i,2], type = "b",   # Cool animation
      xlab = "Intercept (alpha)", ylab = "Slope (beta)",
      main = "MCMC trajectory (red: truth, blue: running sample means)")

    abline(v = true.vals[1], col = "red", lwd = 1)
    abline(h = true.vals[2], col = "red", lwd = 1)
    abline(h = mean(out[1:i,2]), col = "blue", lwd = 1)
    abline(v = mean(out[1:i,1]), col = "blue", lwd = 1)
    points(out[1,1], out[1,2])

  }
  # Compute postburnin statistics
  pmeans <- apply(out[nburn:niter,], 2, mean) # posterior mean
  psds <- apply(out[nburn:niter,], 2, sd)     # posterior sd
  CRIs <- apply(out[nburn:niter,], 2,         # 95% CRI
    function(x) quantile(x, c(0.025, 0.975)))

  # Compute acceptance probabilities (over all draws, including pre-burnin)
#  acc.prob1 <- acc.counter1/niter
#  acc.prob2 <- acc.counter2/niter
   acc.prob1 <- mean(acc[nburn:niter, 1])
   acc.prob2 <- mean(acc[nburn:niter, 2])

  if(show.plots){
  # Plot top: traceplot without burnin
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(2, 2), mar = c(6,6,5,3), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  plot(1:niter, out[,1], main = 'alpha (all MCMC draws)', xlab = "MC iteration", ylab = 'Posterior draw', type = 'l')
  abline(h = true.vals[1], lwd = 2, lty = 3, col = 'red')
  plot(1:niter, out[,2], main = 'beta (all MCMC draws)', xlab = "MC iteration", ylab = 'Posterior draw', type = 'l')
  abline(h = true.vals[2], lwd = 2, lty = 3, col = 'red')

  # Plot bottom: traceplot with burnin
  plot((nburn+1):niter, out[(nburn+1):niter,1], main = 'alpha (post-burnin)', xlab = "MC iteration", ylab = 'Posterior draw', type = 'l')
  abline(h = true.vals[1], lwd = 2, lty = 3, col = 'red')
  plot((nburn+1):niter, out[(nburn+1):niter,2], main = 'beta (post-burnin)', xlab = "MC iteration", ylab = 'Posterior draw', type = 'l')
  abline(h = true.vals[2], lwd = 2, lty = 3, col = 'red')
  
  # Also print out the posterior mean, sd and 95% CRI (post-burnin)
  tmp.tab <- array(NA, dim = c(2, 4), dimnames = list(c('alpha', 'beta'), c('Post.mean', 'Post.sd', '2.5%', '97.5%'))) 
  tmp.tab[,1] <- round(pmeans, 4)
  tmp.tab[,2] <- round(psds, 4)
  tmp.tab[,3:4] <- CRIs
  tmp.tab 
 }
 if (!quiet) {
   message("\nAcceptance prob. (post-burnin) for alpha: ", round(acc.prob1, 2))
   message("Acceptance prob. (post-burnin) for beta: ", round(acc.prob2, 2))
   end.time <- Sys.time()
   elapsed.time <- round(difftime(end.time, start.time, units = "secs"), 2)
   message(paste(niter-nburn, "post-burnin posterior draws produced in", elapsed.time, "seconds\n"))
}
 # Numerical output 
  return(list(niter = niter, nburn = nburn, post.summary = tmp.tab,
    acc.prob1 = acc.prob1, acc.prob2 = acc.prob2))
} # ------------ End of function definition ------------------------

