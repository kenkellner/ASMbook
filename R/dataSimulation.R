#' Simulate data for Chapter 4: Model of the mean
#'
#' Simulate body mass measurements for 'n' peregrine falcons
#' from a normal distribution with population mean = 'mean' and population sd = 'sd'
#'
#' @param n The sample size
#' @param mean Population mean
#' @param sd Population standard deviation
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{n}{Sample size}
#'   \item{mean}{Population mean}
#'   \item{sd}{Population SD}
#'   \item{y}{Simulated peregrine mass measurements}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat4())          # Implicit default arguments
#' str(dat <- simDat4(n = 10^6))  # More than the world population of peregrines
#' str(dat <- simDat4(n = 10, mean = 900, sd = 40))  # Simulate 10 female peregrines
#'
#' @importFrom graphics abline hist
#' @importFrom stats rnorm
#' @export
simDat4 <- function(n = 10, mean = 600, sd = 30){
  y <- rnorm(n = n, mean = mean, sd = sd)         # Simulate values
  hist(y, col = 'grey', main = paste("Body mass of", n, "peregrine falcons")) # Plot
  abline(v = mean, col = 'red', lwd = 3)          # Add population mean
  return(list(n = n, mean = mean, sd = sd, y = y))
}

#' Simulate data for Chapter 5: Simple linear regression
#'
#' Simulate percent occupancy population trajectory of Swiss Wallcreepers
#' from a normal distribution. Note that other choices of arguments may
#' lead to values for x and y that no longer make sense in the light of
#' the story in Chapter 5 (i.e., where y is a percentage), but will
#' still be OK for the statistical model in that chapter.
#'
#' @param n The sample size
#' @param a Value for the intercept
#' @param b Value for the slope
#' @param sigma2 Value for the residual variance
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{n}{Sample size}
#'   \item{a}{Intercept}
#'   \item{b}{Slope}
#'   \item{sd}{Residual SD}
#'   \item{y}{Simulated wallcreeper occupancy probabilities}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat5())        # Implicit default arguments
#' str(dat <- simDat5(b = 0))   # Stable population (this is a de-facto "model-of-the-mean")
#' str(dat <- simDat5(b = 0.5)) # Expected increase
#'
#' @importFrom stats rnorm
#' @export
simDat5 <- function(n = 16, a = 40, b = -0.5, sigma2 = 25){
  x <- 1:n                                               # Covariate x
  y <- rnorm(n = n, mean = a + b * x, sd = sqrt(sigma2)) # Percentage occupied sites
  plot((x+1989), y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.5, pch = 16, col = rgb(0,0,0,0.6), frame = FALSE) # plot
  return(list(n = n, a = a, b = b, sd = sqrt(sigma2), y = y))
}

#' Simulate data for Chapter 6.2: Two groups with equal variance
#'
#' Simulate wingspan measurements in female and male peregrines.
#'
#' @param n1 The sample size of females
#' @param n2 The sample size of males
#' @param mu1 The population mean males
#' @param mu2 The population mean females
#' @param sigma The standard deviation for both groups
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{n1}{Female sample size}
#'   \item{n2}{Male sample size}
#'   \item{mu1}{Female mean}
#'   \item{mu2}{Male mean}
#'   \item{beta}{Difference in wingspan mean between sexes}
#'   \item{sigma}{Standard deviation for both groups}
#'   \item{x}{Indicator variable for sex, 1 = male}
#'   \item{y}{Simulated wingspan data}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat62())      # Implicit default arguments
#' str(dat <- simDat62(n1 = 1000, n2 = 10000)) # Much larger sample sizes
#' 
#' # Revert to "model-of-the-mean" (with larger sample size)
#' str(dat <- simDat62(n1 = 10000, n2 = 10000, mu1 = 105, mu2 = 105))
#'
#' @importFrom graphics boxplot
#' @importFrom stats rnorm
#' @export
simDat62 <- function(n1 = 60, n2 = 40, mu1 = 105, mu2 = 77.5, sigma = 2.75){
  y1 <- rnorm(n1, mu1, sigma)     # Data for females
  y2 <- rnorm(n2, mu2, sigma)     # Data for males
  y <- c(y1, y2)                  # Merge both
  beta <- mu1 - mu2               # Sex difference in the mean
  x <- rep(c(0,1), c(n1, n2))     # Indicator variable indexing a male
  boxplot(y ~ x, col = "grey", xlab = "Indicator for Male", ylab = "Wingspan (cm)", las = 1, frame = FALSE)
  return(list(n1 = n1, n2 = n2, mu1 = mu1, mu2 = mu2, beta = beta, sigma = sigma, x = x, y = y))
}

#' Simulate data for Chapter 6.3: Two groups with unequal variance
#'
#' Simulate wingspan measurements in female and male peregrines.
#'
#' @param n1 The sample size of females
#' @param n2 The sample size of males
#' @param mu1 The population mean males
#' @param mu2 The population mean females
#' @param sigma1 The standard deviation for females
#' @param sigma2 The standard deviation for males
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{n1}{Female sample size}
#'   \item{n2}{Male sample size}
#'   \item{mu1}{Female mean}
#'   \item{mu2}{Male mean}
#'   \item{beta}{Difference in wingspan mean between sexes}
#'   \item{sigma1}{Standard deviation for females}
#'   \item{sigma2}{Standard deviation for males}
#'   \item{x}{Indicator variable for sex, 1 = male}
#'   \item{y}{Simulated wingspan data}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat63())            # Implicit default arguments
#' str(dat <- simDat63(sigma1 = 5, sigma2 = 1)) # Very unequal variances
#' 
#' # Much larger sample sizes and larger difference in residual variation
#' str(dat <- simDat63(n1 = 10000, n2 = 10000, sigma1 = 5, sigma2 = 2))
#' 
#' # Revert to model with homoscedasticity
#' str(dat <- simDat63(n1 = 10000, n2 = 10000, sigma1 = 5, sigma2 = 5))
#' 
#' # Revert to "model-of-the-mean" (with larger sample size)
#' str(dat <- simDat63(n1 = 10000, n2 = 10000, mu1 = 105, mu2 = 105, sigma1 = 5, sigma2 = 5))
#'
#' @importFrom graphics boxplot
#' @importFrom stats rnorm
#' @export
simDat63 <- function(n1 = 60, n2 = 40, mu1 = 105, mu2 = 77.5, sigma1 = 3, sigma2 = 2.5){
  y1 <- rnorm(n1, mu1, sigma1)    # Data for females
  y2 <- rnorm(n2, mu2, sigma2)    # Data for males
  y <- c(y1, y2)                  # Merge both
  beta <- mu1 - mu2               # Sex difference in mean
  x <- rep(c(0,1), c(n1, n2))     # Indicator variable indexing a male
  boxplot(y ~ x, col = "grey", xlab = "Indicator for Male", ylab = "Wingspan (cm)", las = 1, frame = FALSE)
  return(list(n1 = n1, n2 = n2, mu1 = mu1, mu2 = mu2, beta = beta,
    sigma1 = sigma1, sigma2 = sigma2, x = x, y = y))
}


#' Simulate data for Chapter 7.2: ANOVA with fixed effects of population
#'
#' Simulate snout-vent length measurements of 'nSample' smooth snakes in each of 'nPops' populations
#' Data are simulated under the assumptions of a model with fixed effects of populations
#'
#' @param nPops Number of populations
#' @param nSample Samples from each population
#' @param pop.means Vector of mean length for each population
#' @param sigma Value for the residual standard deviation
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nSample}{Number of samples per population}
#'   \item{pop.means}{Population means}
#'   \item{sigma}{Residual SD}
#'   \item{pop}{Indicator for population number}
#'   \item{eps}{Simulated residuals}
#'   \item{y}{Simulated lengths}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat72())      # Implicit default arguments
#' str(dat <- simDat72(nPops = 10, nSample = 5, pop.means = runif(10,20,60))) # More pops, fewer snakes in each
#' 
#' # Revert to "model-of-the-mean" (larger sample size to minimize sampling variability)
#' str(dat <- simDat72(nSample = 1000, pop.means = rep(50, 5), sigma = 5))
#'
#' @importFrom graphics boxplot par
#' @importFrom stats model.matrix rnorm
#' @export
simDat72 <- function(nPops = 5, nSample = 10, pop.means = c(50, 40, 45, 55, 60), sigma = 5){
  stopifnot(length(pop.means) == nPops)
  n <- nPops * nSample                       # Total number of data points
  eps <- rnorm(n, 0, sigma)                  # Residuals 
  pop <- factor(rep(1:nPops, rep(nSample, nPops))) # Indicator for population
  means <- rep(pop.means, rep(nSample, nPops))
  X <- as.matrix(model.matrix(~ pop-1))      # Create design matrix
  y <- as.numeric(X %*% as.matrix(pop.means) + eps) # %*% denotes matrix multiplication
  oldpar <- par()
  par(mar = c(6,6,6,3), cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
  boxplot(y ~ pop, col = "grey", xlab = "Population", ylab = "SVL", main = "", las = 1, frame = FALSE)
  par(oldpar)
  return(list(nPops = nPops, nSample = nSample, pop.means = pop.means, sigma = sigma, 
    pop = pop, eps = eps, y = y))
}

#' Simulate data for Chapter 7.3: ANOVA with random effects of population
#'
#' Simulate snout-vent length measurements of 'nSample' smooth snakes in each of 'nPops' populations
#' Data are simulated under the assumptions of a model with random effects of populations
#'
#' @param nPops Number of populations
#' @param nSample Samples from each population
#' @param pop.grand.mean Mean of population means (hyperparameter)
#' @param pop.sd Standard deviation of population means (hyperparameter)
#' @param sigma Value for the residual standard deviation
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nSample}{Number of samples per population}
#'   \item{pop.grand.mean}{Mean of population means}
#'   \item{pop.sd}{SD of population means}
#'   \item{sigma}{Residual SD}
#'   \item{pop}{Indicator for population number}
#'   \item{pop.means}{Simulated population means}
#'   \item{eps}{Simulated residuals}
#'   \item{y}{Simulated lengths}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat73())      # Implicit default arguments
#' # More pops, more snakes in each, more among-population variability
#' str(dat <- simDat73(nPops = 20, nSample = 30, pop.sd = 8)) 
#' 
#' # Revert to "model-of-the-mean" (larger sample size to minimize sampling variability)
#' str(dat <- simDat73(nSample = 1000, pop.sd = 0, sigma = 5))
#'
#' @importFrom graphics abline par
#' @importFrom stats model.matrix rnorm
#' @export
simDat73 <- function(nPops = 10, nSample = 12, pop.grand.mean = 50, pop.sd = 3, sigma = 5){
  n <- nPops * nSample                       # Total number of data points
  pop <- factor(rep(1:nPops, rep(nSample, nPops))) # Indicator for population
  pop.means <- rnorm(n = nPops, mean = pop.grand.mean, sd = pop.sd) # Create population means
  eps <- rnorm(n, 0, sigma)                  # Residuals 
  X <- as.matrix(model.matrix(~ pop-1))      # Create design matrix
  y <- as.numeric(X %*% as.matrix(pop.means) + eps) # %*% denotes matrix multiplication
  oldpar <- par()
  par(mar = c(6,6,6,3), cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
  boxplot(y ~ pop, col = "grey", xlab = "Population", ylab = "SVL", main = "", las = 1, frame = FALSE)
  abline(h = pop.grand.mean)
  par(oldpar)
  return(list(nPops = nPops, nSample = nSample, pop.grand.mean = pop.grand.mean,
    pop.sd = pop.sd, sigma = sigma, pop = pop, pop.means = pop.means, eps = eps, y = y))
}

#' Simulate data for Chapter 8: Two-way ANOVA
#'
#' Simulate wing length measurements of mourning cloak butterflies with two factors (habitat and population)
#' including their interaction if so wished (simulation under a fixed-effects model)
#'
#' @param nPops Number of populations
#' @param nHab Number of habitats
#' @param nSample Samples from each population-habitat combination
#' @param baseline Grand mean length
#' @param pop.eff Population effects, should be nPops - 1 values
#' @param hab.eff Habitat effects, should be nHab - 1 values
#' @param interaction.eff Interaction effects, should be (nPops-1)*(nHab-1) values
#' @param sigma Value for the residual standard deviation
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nSample}{Number of samples per population}
#'   \item{baseline}{Grand mean length}
#'   \item{pop.eff}{Population effects}
#'   \item{hab.eff}{Habitat effects}
#'   \item{interaction.eff}{Interaction effects}
#'   \item{sigma}{Residual SD}
#'   \item{all.eff}{All effects}
#'   \item{pop}{Indicator for population number}
#'   \item{hab}{Indicator for habitat number}
#'   \item{eps}{Simulated residuals}
#'   \item{wing}{Simulated wing lengths}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat8())      # Implicit default arguments (for the model with interactions)
#' # Model with main effects only (and very large sample size; to minimize sampling error 
#' # and clarify structure of main effects in plot)
#' str(dat <- simDat8(nSample = 1000, interaction.eff = c(0,0,0,0, 0,0,0,0)))
#' str(dat <- simDat8(nSample = 10000, interaction.eff = rep(0, 8))) # same, even larger sample size
#' 
#' # Revert to one-way ANOVA model with only effects of pop (with much larger sample size)
#' str(dat <- simDat8(nSample = 10000, pop.eff = c(-10, -5, 5, 10),
#'   hab.eff = c(0, 0), interaction.eff = rep(0, 8)))  # note no effect of habitat
#' 
#' # Revert to one-way ANOVA model with only effects of hab
#' str(dat <- simDat8(nSample = 10000, pop.eff = c(0, 0, 0, 0),
#'   hab.eff = c(5, 10), interaction.eff = rep(0, 8)))  # note no effect of pop
#' 
#' # Revert to "model-of-the-mean"
#' str(dat <- simDat8(nSample = 10000, pop.eff = c(0, 0, 0, 0), 
#'   hab.eff = c(0, 0), interaction.eff = rep(0, 8)))  # note no effect of pop nor of h
#'
#' @importFrom graphics abline
#' @importFrom stats model.matrix rnorm
#' @export
simDat8 <- function(nPops = 5, nHab = 3, nSample = 12, baseline = 40, pop.eff = c(-10, -5, 5, 10),
  hab.eff = c(5, 10), interaction.eff = c(-2, 3, 0, 4, 4, 0, 3, -2), sigma = 3){
  n <- nPops * nSample 
  all.eff <- c(baseline, pop.eff, hab.eff, interaction.eff)
  pop <- gl(n = nPops, k = nSample, length = n)
  hab <- gl(n = nHab, k = nSample / nHab, length = n)
  eps <- rnorm(n, 0, sigma)
  Xmat <- as.matrix(model.matrix(~ pop * hab) )
  wing <- as.vector(Xmat %*% all.eff + eps)
  boxplot(wing ~ hab*pop, col = "grey", xlab = "Habitat-by-Population combination",
    ylab = "Wing length", main = "Simulated data set", las = 1, ylim = c(20, 70), frame = FALSE)
  abline(h = 40)
  return(list(nPops = nPops, nSample = nSample, baseline = baseline, pop.eff = pop.eff, 
  hab.eff = hab.eff, interaction.eff = interaction.eff, sigma = sigma, all.eff = all.eff,
  pop = pop, hab = hab, eps = eps, wing = wing))
}

#' Simulate data for Chapter 9: ANCOVA or general linear model
#'
#' Simulate mass ~ length regressions in 3 populations of asp vipers
#'
#' @param nPops Number of populations
#' @param nSample Samples from each population
#' @param beta.vec Vector of regression parameter values
#' @param sigma Value for the residual standard deviation
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nSample}{Number of samples per population}
#'   \item{beta.vec}{Regression parameter values}
#'   \item{sigma}{Residual SD}
#'   \item{x}{Indicator for population number}
#'   \item{pop}{Population name (factor)}
#'   \item{lengthC}{Centered body length for each viper}
#'   \item{mass}{Simulated body mass for each viper}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' # Implicit default arguments (with interaction of length and pop)
#' str(dat <- simDat9())
#'
#' # Revert to main-effects model with parallel lines
#' str(dat <- simDat9(beta.vec = c(80, -30, -20, 6, 0, 0)))
#'
#' # Revert to main-effects model with parallel lines 
#' # (larger sample size to better show patterns)
#' str(dat <- simDat9(nSample = 100, beta.vec = c(80, -30, -20, 6, 0, 0)))
#' 
#' # Revert to simple linear regression: no effect of population 
#' # (larger sample size to better show patterns)
#' str(dat <- simDat9(nSample = 100, beta.vec = c(80, 0, 0, 6, 0, 0)))
#'
#' # Revert to one-way ANOVA model: no effect of body length 
#' # (larger sample size to better show patterns)
#' str(dat <- simDat9(nSample = 100, beta.vec = c(80, -30, -20, 0, 0, 0)))
#'
#' # Revert to "model-of-the-mean": no effects of either body length or population)
#' str(dat <- simDat9(nSample = 100, beta.vec = c(80, 0, 0, 0, 0, 0)))
#'
#' @importFrom graphics matplot par
#' @importFrom stats model.matrix rnorm runif
#' @export
simDat9 <- function(nPops = 3, nSample = 10, beta.vec = c(80, -30, -20, 6, -3, -4), sigma = 10){
  n <- nPops * nSample
  x <- rep(1:nPops, rep(nSample, nPops))
  pop <- factor(x, labels = c("Pyrenees", "Massif Central", "Jura"))
  length <- runif(n, 45, 70)     # Body length
  lengthC <- length-mean(length) # Same centered
  Xmat <- model.matrix(~ pop * lengthC)
  mass <- as.numeric(Xmat[,] %*% beta.vec + rnorm(n = n, mean = 0, sd = sigma))
  oldpar <- par()
  par(mar = c(5,5,4,2), cex.lab = 1.5, cex.axis = 1.5)
  matplot(cbind(length[1:nSample], length[(nSample+1):(2*nSample)], length[(2*nSample+1):(3*nSample)]),
    cbind(mass[1:nSample], mass[(nSample+1):(2*nSample)], mass[(2*nSample+1):(3*nSample)]),
    ylim = c(0, max(mass)), ylab = "Body mass (g)", xlab = "Body length (cm)", 
	col = c("Red","Green","Blue"), pch = c("P", "M", "J"), las = 1, cex = 1.6, cex.lab = 1.5, frame = FALSE)
  par(oldpar)
  return(list(nPops = nPops, nSample = nSample, beta.vec = beta.vec, sigma = sigma, x = x, pop = pop,
    lengthC = lengthC, mass = mass))
}


#' Simulate data for Chapter 10.2: Linear mixed-effects model
#'
#' Simulate mass ~ length regressions in 56 populations of snakes
#' with random population effects for intercepts and slopes.
#' There is NO correlation between the intercept and slope random variables.
#'
#' @param nPops Number of populations
#' @param nSample Samples from each population
#' @param mu.alpha Mean of random intercepts
#' @param sigma.alpha SD of random intercepts
#' @param mu.beta Mean of random slopes
#' @param sigma.beta SD of random slopes
#' @param sigma Residual standard deviation
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nSample}{Number of samples per population}
#'   \item{mu.alpha}{Mean of random intercepts}
#'   \item{sigma.alpha}{SD of random intercepts}
#'   \item{mu.beta}{Mean of random slopes}
#'   \item{sigma.beta}{SD of random slopes}
#'   \item{sigma}{Residual SD}
#'   \item{pop}{Indicator for population number}
#'   \item{orig.length}{Snake body length, not standardized}
#'   \item{lengthN}{Snake body length, standardized}
#'   \item{alpha}{Random intercepts}
#'   \item{beta}{Random slopes}
#'   \item{eps}{Residuals}
#'   \item{mass}{Simulated body mass for each snake}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' library(lattice)
#' str(dat <- simDat102())      # Implicit default arguments
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#'        main = 'Realized mass-length relationships', pch = 16, cex = 1.2, 
#'        col = rgb(0, 0, 0, 0.4))
#' 
#' # Fewer populations, more snakes (makes patterns perhaps easier to see ?)
#' str(dat <- simDat102(nPops = 16, nSample = 100))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#'        main = 'Realized mass-length relationships\n(default random-coefficients model)', 
#'        pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to random intercept model (and less residual variation), fewer pops 
#' # and more snakes. Increased sigma.alpha to emphasize the random intercepts part
#' str(dat <- simDat102(nPops = 16, nSample = 100, sigma.alpha = 50, sigma.beta = 0, sigma = 10))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#'        main = 'Realized mass-length relationships (random-intercepts model)', 
#'        pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to random-effects one-way ANOVA model, only random intercepts, but zero slopes
#' str(dat <- simDat102(nPops = 16, nSample = 100, sigma.alpha = 50, mu.beta = 0, sigma.beta = 0, sigma = 10))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#'        main = 'Realized mass-length relationships\n(one-way ANOVA model with random pop effects)', 
#'        pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to simple linear regression (= no effects of pop on either intercepts or slopes)
#' str(dat <- simDat102(nPops = 16, nSample = 100, sigma.alpha = 0, sigma.beta = 0, sigma = 10))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#'        main = 'Realized mass-length relationships\n(this is de-facto a simple linear regression now)', 
#'        pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to "model-of-the-mean": no effects of either population or body length
#' str(dat <- simDat102(nPops = 16, nSample = 100, sigma.alpha = 0, mu.beta = 0, sigma.beta = 0, sigma = 10))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#'        main = 'Realized mass-length relationships\n("model-of-the-mean" without any effects of pop or length)', 
#'        pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#' 
#' @importFrom lattice xyplot
#' @importFrom stats model.matrix rnorm runif
#' @export
simDat102 <- function(nPops = 56, nSample = 10, mu.alpha = 260, sigma.alpha = 20, mu.beta = 60,
  sigma.beta = 30, sigma = 30){
  n <- nPops * nSample
  pop <- gl(n = nPops, k = nSample)
  orig.length <- runif(n, 45, 70)
  mn <- mean(orig.length)
  sd <- sd(orig.length)
  lengthN <- (orig.length - mn) / sd
  Xmat <- model.matrix(~ pop * lengthN - 1 - lengthN) 
  alpha <- rnorm(n = nPops, mean = mu.alpha, sd = sigma.alpha)
  beta <- rnorm(n = nPops, mean = mu.beta, sd = sigma.beta)
  all.pars <- c(alpha, beta)
  lin.pred <- Xmat[,] %*% all.pars
  eps <- rnorm(n = n, mean = 0, sd = sigma)
  mass <- as.numeric(lin.pred + eps)
  xyplot(mass ~ lengthN | pop, xlab = 'Length', ylab = 'Mass', main = 'Realized mass-length relationships', pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))     # %%%%% FOR SOME REASON NO PLOT IS PRODUCED %%%%%%
  return(list(nPops = nPops, nSample = nSample, mu.alpha = mu.alpha, sigma.alpha = sigma.alpha, 
    mu.beta = mu.beta, sigma.beta = sigma.beta, sigma = sigma, pop = pop, orig.length = orig.length, 
	lengthN = lengthN, alpha = alpha, beta = beta, eps = eps, mass = mass))
}


#' Simulate data for Chapter 10.5: Linear mixed-effects model with correlation between intercepts and slopes
#'
#' Simulate mass ~ length regressions in 56 populations of snakes
#' with random population effects for intercepts and slopes.
#' NOTE that now there IS a correlation between the intercept and slope random variables.
#'
#' @param nPops Number of populations
#' @param nSample Samples from each population
#' @param mu.alpha Mean of random intercepts
#' @param sigma.alpha SD of random intercepts
#' @param mu.beta Mean of random slopes
#' @param sigma.beta SD of random slopes
#' @param cov.alpha.beta Covariance between alpha and beta
#' @param sigma Residual standard deviation
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nSample}{Number of samples per population}
#'   \item{mu.alpha}{Mean of random intercepts}
#'   \item{sigma.alpha}{SD of random intercepts}
#'   \item{mu.beta}{Mean of random slopes}
#'   \item{sigma.beta}{SD of random slopes}
#'   \item{cov.alpha.beta}{Covariance betwen alpha and beta}
#'   \item{sigma}{Residual SD}
#'   \item{pop}{Indicator for population number}
#'   \item{orig.length}{Snake body length, not standardized}
#'   \item{lengthN}{Snake body length, standardized}
#'   \item{ranef.matrix}{Random effects matrix}
#'   \item{alpha}{Random intercepts}
#'   \item{beta}{Random slopes}
#'   \item{eps}{Residuals}
#'   \item{mass}{Simulated body mass for each snake}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' library(lattice)
#' str(dat <- simDat105())      # Implicit default arguments
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#' main = 'Realized mass-length relationships', pch = 16, cex = 1.2, 
#' col = rgb(0, 0, 0, 0.4)) # This works
#' 
#' # Fewer populations, more snakes (makes patterns perhaps easier to see)
#' str(dat <- simDat105(nPops = 16, nSample = 100))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#' main = 'Realized mass-length relationships (random-coefficients model\nwith intercept-slope correlation)', 
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#' 
#' # Revert to simpler random-coefficient model without correlation between intercepts and slopes
#' # (that means to set to zero the covariance term)
#' str(dat <- simDat105(nPops = 16, nSample = 100, cov.alpha.beta = 0))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#' main = 'Realized mass-length relationships\n(random-coefficients model without correlation)', 
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to even simpler random-intercepts model without correlation between intercepts and slopes
#' # (that means to set to zero the covariance term as well as the among-population variance of the slopes)
#' # Note that sigma.beta = 0 and non-zero covariance crashes owing to non-positive-definite VC matrix
#' str(dat <- simDat105(nPops = 16, nSample = 100, sigma.alpha = 50, sigma.beta = 0, cov.alpha.beta = 0, 
#' sigma = 10))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#' main = 'Realized mass-length relationships\n(random-intercepts model)', 
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4)) # This works
#'
#' # Revert to random-effects one-way ANOVA model, only random intercepts, but zero slopes
#' str(dat <- simDat105(nPops = 16, nSample = 100, sigma.alpha = 50, mu.beta = 0, sigma.beta = 0, 
#'   cov.alpha.beta = 0, sigma = 10))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#' main = 'Realized mass-length relationships\n(one-way ANOVA model with random pop effects)', 
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to simple linear regression (= no effects of pop on either intercepts or slopes)
#' str(dat <- simDat105(nPops = 16, nSample = 100, sigma.alpha = 0, sigma.beta = 0, 
#'   cov.alpha.beta = 0, sigma = 10))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#' main = 'Realized mass-length relationships\n(this is de-facto a simple linear regression now)', 
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to "model-of-the-mean": no effects of either population or body length
#' str(dat <- simDat105(nPops = 16, nSample = 100, sigma.alpha = 0, mu.beta = 0, sigma.beta = 0,
#'   cov.alpha.beta = 0, sigma = 10))
#' xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
#' main = 'Realized mass-length relationships\n("model-of-the-mean" without any effects of pop or length)', 
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#' 
#' @importFrom lattice xyplot
#' @importFrom MASS mvrnorm
#' @importFrom grDevices rgb
#' @importFrom stats model.matrix rnorm runif
#' @export
simDat105 <- function(nPops = 56, nSample = 10, mu.alpha = 260, sigma.alpha = 20,
  mu.beta = 60, sigma.beta = 30, cov.alpha.beta = -50, sigma = 30){
  n <- nPops * nSample
  pop <- gl(n = nPops, k = nSample)
  orig.length <- runif(n, 45, 70)
  mn <- mean(orig.length)
  sd <- sd(orig.length)
  lengthN <- (orig.length - mn) / sd
  Xmat <- model.matrix(~ pop * lengthN - 1 - lengthN) 
  mu.vector <- c(mu.alpha, mu.beta)
  VC.matrix <- matrix(c(sigma.alpha^2, cov.alpha.beta, cov.alpha.beta, sigma.beta^2),2,2)
  ranef.matrix <- mvrnorm(n = nPops, mu = mu.vector, Sigma = VC.matrix)
  colnames(ranef.matrix) <- c("alpha", "beta")
  lin.pred <- Xmat[,] %*% as.vector(ranef.matrix)
  eps <- rnorm(n = n, mean = 0, sd = sigma) 
  mass <- as.numeric(lin.pred + eps)
  xyplot(mass ~ lengthN | pop, xlab = 'Length', ylab = 'Mass', main = 'Realized mass-length relationships', pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))  # %%%%% FOR SOME REASON NO PLOT IS PRODUCED
  return(list(nPops = nPops, nSample = nSample, mu.alpha = mu.alpha, sigma.alpha = sigma.alpha, 
    mu.beta = mu.beta, sigma.beta = sigma.beta, cov.alpha.beta = cov.alpha.beta, sigma = sigma,
	pop = pop, orig.length = orig.length, lengthN = lengthN, ranef.matrix = ranef.matrix, eps = eps, mass = mass))
}
