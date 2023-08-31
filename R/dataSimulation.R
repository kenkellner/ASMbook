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
  par(mar = c(6,6,6,3), cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
  boxplot(y ~ pop, col = "grey", xlab = "Population", ylab = "SVL", main = "", las = 1, frame = FALSE)
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
  par(mar = c(6,6,6,3), cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
  boxplot(y ~ pop, col = "grey", xlab = "Population", ylab = "SVL", main = "", las = 1, frame = FALSE)
  abline(h = pop.grand.mean)
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
  par(mar = c(5,5,4,2), cex.lab = 1.5, cex.axis = 1.5)
  matplot(cbind(length[1:nSample], length[(nSample+1):(2*nSample)], length[(2*nSample+1):(3*nSample)]),
    cbind(mass[1:nSample], mass[(nSample+1):(2*nSample)], mass[(2*nSample+1):(3*nSample)]),
    ylim = c(0, max(mass)), ylab = "Body mass (g)", xlab = "Body length (cm)", 
	col = c("Red","Green","Blue"), pch = c("P", "M", "J"), las = 1, cex = 1.6, cex.lab = 1.5, frame = FALSE)
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


#' Simulate data for Chapter 11: Comparing two groups of Poisson counts
#'
#' Generate counts of hares in two areas with different landuse
#'
#' @param nSites Number of sites
#' @param alpha Intercept
#' @param beta Slope for land use
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nSites}{Number of sites}
#'   \item{alpha}{Intercept}
#'   \item{beta}{Slope for land use}
#'   \item{y}{Simulated hare counts}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat11())      # Implicit default arguments
#'
#' # Revert to "Poisson model-of-the-mean" 
#' # (Increase sample size to reduce sampling variability)
#' str(dat <- simDat11(nSites = 1000, beta = 0)) 
#'
#' @importFrom graphics boxplot
#' @importFrom stats rpois
#' @export
simDat11 <- function(nSites = 30, alpha = log(2), beta = log(5)-log(2)){
  x <- gl(n = 2, k = nSites, labels = c("grassland", "arable"))
  n <- 2 * nSites
  lambda <- exp(alpha + beta*(as.numeric(x)-1)) 
  y <- rpois(n = n, lambda = lambda)
  y[c(1, 10, 35)] <- NA            # Turn some observations into NAs
  boxplot(y ~ x, col = "grey", xlab = "Land-use", ylab = "Hare count", las = 1, frame = FALSE)
  return(list(nSites = nSites, alpha = alpha, beta = beta, y = y))
}


#' Simulate data for Chapter 12.2: Overdispersed counts
#'
#' Generate counts of hares in two landuse types
#' when there may be overdispersion relative to a Poisson
#'
#' @param nSites Number of sites
#' @param alpha Intercept
#' @param beta Slope for land use
#' @param sd Standard deviation for overdispersion
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nSites}{Number of sites}
#'   \item{alpha}{Intercept}
#'   \item{beta}{Slope for land use}
#'   \item{sd}{Standard deviation for overdispersion}
#'   \item{C_OD}{Simulated hare counts with overdispersion}
#'   \item{C_Poisson}{Simulated hare counts without overdispersion}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat122())      # Implicit default arguments
#' 
#' # Much greater OD to emphasize patterns (also larger sample size)
#' str(dat <- simDat122(nSites = 100, sd = 1))
#' 
#' # Revert to "Poisson model-of-the-mean" (i.e., without an effect of landuse type)
#' str(dat <- simDat122(nSites = 100, beta = 0, sd = 1))
#'
#' @importFrom graphics boxplot par
#' @importFrom stats rnorm rpois
#' @export
simDat122 <- function(nSites = 50, alpha = log(2), beta = log(5)-log(2), sd = 0.5){
  x <- gl(n = 2, k = nSites, labels = c("grassland", "arable"))
  n <- 2 * nSites
  eps <- rnorm(2*nSites, mean = 0, sd = sd)     # Normal random effect to create overdispersion
  lambda.OD <- exp(alpha + beta*(as.numeric(x)-1) + eps)
  lambda.Poisson <- exp(alpha + beta*(as.numeric(x)-1))
  C_OD <- rpois(n = n, lambda = lambda.OD)           # Counts with OD
  C_Poisson <- rpois(n = n, lambda = lambda.Poisson) # Counts without OD
  par(mfrow = c(1,2))
  boxplot(C_OD ~ x, col = "grey", xlab = "Land-use", main = "With overdispersion", ylab = "Hare count", las = 1, ylim = c(0, max(C_OD)), frame = FALSE)
  boxplot(C_Poisson ~ x, col = "grey", xlab = "Land-use", main = "Without overdispersion", ylab = "Hare count", las = 1, ylim = c(0, max(C_OD)) , frame = FALSE )
  return(list(nSites = nSites, alpha = alpha, beta = beta, sd = sd, 
    C_OD = C_OD, C_Poisson = C_Poisson))
}


#' Simulate data for Chapter 12.3: Zero-inflated counts
#'
#' Generate counts of hares in two landuse types when there may be 
#' zero-inflation (this is a simple general hierarchical model, see Chap 19 in the book)
#'
#' @param nSites Number of sites
#' @param alpha Intercept
#' @param beta Slope for land use
#' @param psi Zero inflation parameter (probability of structural 0)
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nSites}{Number of sites}
#'   \item{alpha}{Intercept}
#'   \item{beta}{Slope for land use}
#'   \item{psi}{Zero inflation parameter}
#'   \item{w}{Indicator that count is not a structural 0}
#'   \item{C}{Simulated hare counts with zero inflation}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat123())      # Implicit default arguments
#' 
#' # Drop zero inflation (and make sample sizes bigger)
#' str(dat <- simDat123(nSites = 1000, psi = 0))     # Note 0 % of the sites have structural zeroes now
#'
#' # Drop zero inflation (and make sample sizes bigger)
#' # Half of all sites have structural zeroes
#' str(dat <- simDat123(nSites = 1000, psi = 0.5))
#'
#' # Revert to "model-of-the-mean" without zero inflation
#' # 0 % of the sites have structural zeroes
#' str(dat <- simDat123(nSites = 1000, beta = 0, psi = 0))
#'
#' # Revert to "model-of-the-mean" with zero inflation
#' # 50 % of the sites have structural zeroes
#' str(dat <- simDat123(nSites = 1000, beta = 0, psi = 0.5))
#'
#' @importFrom graphics boxplot
#' @importFrom stats rpois rbinom
#' @export
simDat123 <- function(nSites = 50, alpha = log(2), beta = log(5)-log(2), psi = 0.2){
  x <- gl(n = 2, k = nSites, labels = c("grassland", "arable"))
  n <- 2 * nSites
  w <- rbinom(n = 2 * nSites, size = 1, prob = 1 - psi)
  lambda <- exp(alpha + beta*(as.numeric(x)-1)) 
  C <- rpois(n = n, lambda = w*lambda)           # Counts with some zero-inflation
  boxplot(C ~ x, col = "grey", xlab = "Land-use", ylab = "Hare count", las = 1, frame = FALSE)
  return(list(nSites = nSites, alpha = alpha, beta = beta, psi = psi, w = w, C = C))
}


#' Simulate data for Chapter 12.4: Counts with offsets
#'
#' Generate counts of hares in two landuse types 
#' when study area size A varies and is used as an offset
#'
#' @param nSites Number of sites
#' @param alpha Intercept
#' @param beta Slope for land use
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nSites}{Number of sites}
#'   \item{alpha}{Intercept}
#'   \item{beta}{Slope for land use}
#'   \item{A}{Site areas}
#'   \item{C}{Simulated hare counts}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat124())      # Implicit default arguments
#' str(dat <- simDat124(nSites = 1000, beta = 0)) # "Model-of-the-mean" without effect of landuse
#' str(dat <- simDat124(nSites = 100, alpha = log(2), beta = -2)) # Grassland better than arable
#'
#' @importFrom graphics boxplot
#' @importFrom stats rpois
#' @export
simDat124 <- function(nSites = 50, alpha = log(2), beta = log(5)-log(2)){
  # Generate counts of hares in two landuse types 
  #   when study area size A varies and is used as an offset
  A <- runif(n = 2 * nSites, 2, 5) # Areas range in size from 2 to 5 km2
  x <- gl(n = 2, k = nSites, labels = c("grassland", "arable"))
  lambda <- A * exp(alpha + beta*(as.numeric(x)-1)) 
  C <- rpois(n = 2 * nSites, lambda = lambda)
  boxplot(C ~ x, col = "grey", xlab = "Land-use", ylab = "Hare count", las = 1, frame = FALSE)
  return(list(nSites = nSites, alpha = alpha, beta = beta, A = A, C = C))
}


#' Simulate data for Chapter 13: Poisson ANCOVA
#'
#' Simulate parasite load ~ size regressions in 3 populations of goldenring dragonflies
#'
#' @param nPops Number of populations
#' @param nSample Number of samples per population
#' @param beta.vec Vector of regression coefficients
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nSample}{Number of samples per population}
#'   \item{beta}{Vector of regression coefficients}
#'   \item{x}{Indicator for population number}
#'   \item{pop}{Population name (factor)}
#'   \item{orig.length}{Wing length, non-centered}
#'   \item{wing.length}{Wing length, centered}
#'   \item{load}{Simulated parasite loads}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat13())      # Implicit default arguments
#'
#' # Revert to main-effects model with parallel lines on the log link scale
#' str(dat <- simDat13(nSample = 100, beta.vec = c(-2, 1, 2, 4, 0, 0)))
#'
#' # Same with less strong regression coefficient
#' str(dat <- simDat13(nSample = 100, beta.vec = c(-2, 1, 2, 3, 0, 0)))
#'
#' # Revert to simple linear Poisson regression: no effect of population (and less strong coefficient)
#' str(dat <- simDat13(nSample = 100, beta.vec = c(-2, 0, 0, 3, 0, 0)))
#'
#' # Revert to one-way ANOVA Poisson model: no effect of wing length
#' # (Choose larger sample size and greater differences in the intercepts to better show patterns)
#' str(dat <- simDat13(nSample = 100, beta.vec = c(-1, 3, 5, 0, 0, 0)))
#'
#' # Revert to Poisson "model-of-the-mean": no effects of either wing length or population
#' # Intercept chosen such that average parasite load is 10
#' str(dat <- simDat13(nSample = 100, beta.vec = c(log(10), 0, 0, 0, 0, 0)))
#' mean(dat$load)        # Average is about 10
#'
#' @importFrom graphics par
#' @importFrom stats model.matrix runif rpois
#' @export
simDat13 <- function(nPops = 3, nSample = 100, beta.vec = c(-2, 1, 2, 4, -2, -5)){
  n <- nPops * nSample
  x <- rep(1:nPops, rep(nSample, nPops))
  pop <- factor(x, labels = c("Pyrenees", "Massif Central", "Jura"))
  orig.length <- runif(n, 4.5, 7.0)      # Wing length (cm)
  wing.length <- orig.length - mean(orig.length) # Center wing length
  Xmat <- model.matrix(~ pop * wing.length)
  lin.pred <- Xmat[,] %*% beta.vec
  lambda <- exp(lin.pred)
  load <- rpois(n = n, lambda = lambda)
  par(mar = c(5,5,4,3), cex.axis = 1.5, cex.lab = 1.5)
  plot(wing.length, load, pch = rep(c("P", "M", "J"), each=nSample), las = 1,
    col = rep(c("Red", "Green", "Blue"), each=nSample), ylab = "Parasite load",
	xlab = "Wing length", cex = 1.2, frame = FALSE) # Crashes unless exactly 3 populations
  return(list(nPops = nPops, nSample = nSample, beta.vec = beta.vec, x = x, pop = pop,
    orig.length = orig.length, wing.length = wing.length, load = load))
}


#' Simulate data for Chapter 14: Poisson GLMM
#'
#' Simulate count ~ year regressions in 16 populations of red-backed shrikes
#'
#' @param nPops Number of populations
#' @param nYears Number of years sampled in each population
#' @param mu.alpha Mean of random intercepts
#' @param sigma.alpha SD of random intercepts
#' @param mu.beta Mean of random slopes
#' @param sigma.beta SD of random slopes
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nYears}{Number of years sampled}
#'   \item{mu.alpha}{Mean of random intercepts}
#'   \item{sigma.alpha}{SD of random intercepts}
#'   \item{mu.beta}{Mean of random slopes}
#'   \item{sigma.beta}{SD of random slopes}
#'   \item{pop}{Population index}
#'   \item{orig.year}{Year values, non-scaled}
#'   \item{year}{Year values, scaled to be between 0 and 1}
#'   \item{alpha}{Random intercepts}
#'   \item{beta}{Random slopes}
#'   \item{C}{Simulated shrike counts}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' library(lattice)
#' str(dat <- simDat14())
#' xyplot(dat$C ~ dat$orig.year | dat$pop, ylab = "Red-backed shrike counts", xlab = "Year", pch = 16,
#' cex = 1.2, col = rgb(0, 0, 0, 0.4), 
#' main = 'Realized population trends\n(random-coefficients model)') # works
#'
#' # Revert to random intercept model. Increased sigma.alpha to emphasize the random intercepts part
#' str(dat <- simDat14(nPops = 16, sigma.alpha = 1, sigma.beta = 0))
#' xyplot(dat$C ~ dat$orig.year | dat$pop, ylab = "Red-backed shrike counts", xlab = "Year",
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4), 
#' main = 'Realized population trends (random-intercepts model)')
#'
#' # Revert to random-effects one-way Poisson ANOVA model: random intercepts, but zero slopes
#' str(dat <- simDat14(nPops = 16, sigma.alpha = 1, mu.beta = 0, sigma.beta = 0))
#' xyplot(dat$C ~ dat$orig.year | dat$pop, ylab = "Red-backed shrike counts", xlab = "Year",
#'  pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4), 
#' main = 'Realized population trends\n(random-effects, one-way Poisson ANOVA model)')
#'
#' # Revert to simple log-linear Poisson regression (= no effects of pop on either intercepts or slopes)
#' str(dat <- simDat14(nPops = 16, sigma.alpha = 0, sigma.beta = 0))
#' xyplot(dat$C ~ dat$orig.year | dat$pop, ylab = "Red-backed shrike counts", 
#' xlab = "Year", pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4), 
#' main = 'Realized population trends\n(simple log-linear Poisson regression)')
#'
#' # Revert to Poisson "model-of-the-mean": no effects of either population or body length
#' str(dat <- simDat14(nPops = 16, sigma.alpha = 0, mu.beta = 0, sigma.beta = 0))
#' xyplot(dat$C ~ dat$orig.year | dat$pop, ylab = "Red-backed shrike counts", 
#' xlab = "Year", pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4), 
#' main = 'Realized population trends\n(Poisson "model-of-the-mean")')
#'
#' @importFrom lattice xyplot
#' @importFrom grDevices rgb
#' @importFrom stats model.matrix rnorm rpois
#' @export
simDat14 <- function(nPops = 16, nYears = 30, mu.alpha = 3, sigma.alpha = 1, mu.beta = -2, sigma.beta = 0.6){
  n <- nPops * nYears
  pop <- gl(n = nPops, k = nYears)
  orig.year <- rep(1:nYears, nPops)
  year <- (orig.year-1) / (nYears-1) # Scale year such that squeezed between 0 and 1
  Xmat <- model.matrix(~ pop * year - 1 - year)
  alpha <- rnorm(n = nPops, mean = mu.alpha, sd = sigma.alpha)
  beta <- rnorm(n = nPops, mean = mu.beta, sd = sigma.beta)
  all.effects <- c(alpha, beta) # All together
  lin.pred <- Xmat[,] %*% all.effects
  C <- rpois(n = n, lambda = exp(lin.pred))
  xyplot(C ~ orig.year | pop, ylab = "Red-backed shrike counts", xlab = "Year",
    pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))  #### %%%% DOES NOT PRODUCE PLOTS FOR SOME REASON
  return(list(nPops = nPops, nYears = nYears, mu.alpha = mu.alpha, sigma.alpha = sigma.alpha, 
    mu.beta = mu.beta, sigma.beta = sigma.beta, pop = pop, orig.year = orig.year, 
	year = year, alpha = alpha, beta = beta, C = C))
}


#' Simulate data for Chapter 15: Comparing two groups of binomial counts
#'
#' Generate presence/absence data for two gentian species (Bernoulli variant)
#'
#' @param N Number of sites
#' @param theta.cr Probability of presence for cross-leaved gentian
#' @param theta.ch Probability of presence for chiltern gentian
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{N}{Number of sites}
#'   \item{theta.cr}{Probability for cross-leaved gentian}
#'   \item{theta.ch}{Probability for chiltern gentian}
#'   \item{y}{Simulated presence/absence data}
#'   \item{species.long}{Species indicator (longform), 1 = chiltern}
#'   \item{C}{Aggregated presence/absence data}
#'   \item{species}{Species indicator for aggregated data}
#'   \item{chiltern}{Effect of chiltern (difference in species intercepts)}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat15())      # Implicit default arguments
#'
#' # Revert to "Binomial model-of-the-mean"
#' # (Increase sample size to reduce sampling variability)
#' str(dat <- simDat15(N = 100, theta.cr = 40/100, theta.ch = 40/100)) 
#'
#' @importFrom graphics axis
#' @importFrom stats rbinom
#' @export
simDat15 <- function(N = 50, theta.cr = 12/50, theta.ch = 38/50){
  # Generate presence/absence data for two gentian species (Bernoulli variant)
  y.cr <- rbinom(N, 1, prob = theta.cr)  ;  y.cr  # Det/nondet data for cross-leaved
  y.ch <- rbinom(N, 1, prob = theta.ch)  ;  y.ch  # ditto for chiltern
  y <- c(y.cr, y.ch)     # Merge the two binary vectors
  species.long <- factor(rep(c(0,1), each = N), labels = c("Cross-leaved", "Chiltern"))
  # Aggregate the binary data to become two binomial counts
  C <- c(sum(y.cr), sum(y.ch))     # Tally up detections
  species <- factor(c(0,1), labels = c("Cross-leaved", "Chiltern"))
  plot(C ~ as.numeric(species), xlim = c(0.8, 2.2), ylim = c(0, N), type = 'h', lend = 'butt',
    lwd = 50, col = 'gray30', axes = FALSE, xlab = 'Species of Gentian')
  axis(1, at = c(1, 2), labels = c("Cross-leaved", "Chiltern"))
  axis(2)
  Intercept <- log(theta.cr/(1-theta.cr))
  chiltern <- (log(theta.ch/(1-theta.ch)) - log(theta.cr/(1-theta.cr)))
  return(list(N = N, theta.cr = theta.cr, theta.ch = theta.ch, y = y, species.long = species.long,
    C = C, species = species, Intercept = Intercept, chiltern = chiltern))
}


#' Simulate data for Chapter 16: Binomial ANCOVA
#'
#' Simulate Number black individuals ~ wetness regressions in adders in 3 regions
#'
#' @param nRegion Number of regions
#' @param nSite Number of sites per region
#' @param beta.vec Vector of regression coefficients
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nRegion}{Number of regions}
#'   \item{nSite}{Number of sites per region}
#'   \item{beta}{Vector of regression coefficients}
#'   \item{x}{Indicator for region number}
#'   \item{region}{Region name (factor)}
#'   \item{wetness}{Wetness covariate}
#'   \item{N}{Number of adders captured at each site}
#'   \item{C}{Number of black adders captured at each site}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat16())      # Implicit default arguments
#'
#' # Revert to main-effects model with parallel lines on the logit link scale
#' # (also larger sample size to better see patterns)
#' str(dat <- simDat16(nSite = 100, beta.vec = c(-4, 1, 2, 6, 0, 0)))
#'
#' # Same with less strong logistic regression coefficient
#' str(dat <- simDat16(nSite = 100, beta.vec = c(-4, 1, 2, 3, 0, 0)))
#'
#' # Revert to simple logit-linear binomial regression: no effect of population (and less strong coefficient)
#' str(dat <- simDat16(nSite = 100, beta.vec = c(-4, 0, 0, 3, 0, 0)))
#'
#' # Revert to one-way ANOVA binomial model: no effect of wetness
#' # (Choose greater differences in the intercepts to better show patterns)
#' str(dat <- simDat16(nSite = 100, beta.vec = c(-2, 2, 3, 0, 0, 0)))
#'
#' # Revert to binomial "model-of-the-mean": no effects of either wetness or population
#' # Intercept chosen such that average proportion of black adders is 0.6
#' str(dat <- simDat16(nSite = 100, beta.vec = c(qlogis(0.6), 0, 0, 0, 0, 0)))
#' mean(dat$C / dat$N)        # Average is about 0.6
#'
#' @importFrom graphics par matplot
#' @importFrom stats model.matrix runif rbinom
#' @export
simDat16 <- function(nRegion = 3, nSite = 10, beta.vec = c(-4, 1, 2, 6, 2, -5)){
  n <- nRegion * nSite
  x <- rep(1:nRegion, rep(nSite, nRegion))
  region <- factor(x, labels = c("Jura", "Black Forest", "Alps"))
  wetness.Jura <- sort(runif(nSite, 0, 1))
  wetness.BlackF <- sort(runif(nSite, 0, 1))
  wetness.Alps <- sort(runif(nSite, 0, 1))
  wetness <- c(wetness.Jura, wetness.BlackF, wetness.Alps)
  N <- round(runif(n, 10, 50) )    # Get discrete uniform values for number captured,N
  Xmat <- model.matrix(~ region * wetness)
  lin.pred <- Xmat[,] %*% beta.vec
  exp.p <- exp(lin.pred) / (1 + exp(lin.pred)) # plogis(lin.pred) is same
  C <- rbinom(n = n, size = N, prob = exp.p)
  par(mfrow = c(1,2), mar = c(5,5,3,1))
  matplot(cbind(wetness[1:nSite], wetness[(nSite+1):(2*nSite)], wetness[(2*nSite+1):(3*nSite)]),
    cbind(exp.p[1:nSite], exp.p[(nSite+1):(2*nSite)], exp.p[(2*nSite+1):(3*nSite)]),
	ylab = "Expected proportion black", xlab = "Wetness index", col = c("red","green","blue"),
	pch = c("J","B","A"), lty = "solid", type = "b", las = 1, cex = 1.2, main = "Expected proportion",
	lwd = 2, frame = FALSE)
  matplot(cbind(wetness[1:nSite], wetness[(nSite+1):(2*nSite)], wetness[(2*nSite+1):(3*nSite)]),
    cbind(C[1:nSite]/N[1:nSite], C[(nSite+1):(2*nSite)]/N[(nSite+1):(2*nSite)], C[(2*nSite+1):(3*nSite)]/N[(2*nSite+1):(3*nSite)]), ylab = "Observed proportion black", xlab = "Wetness index",
	col = c("red","green","blue"), pch = c("J","B","A"), las = 1, cex = 1.2, main = "Realized proportion",
	frame = FALSE)
  return(list(nRegion = nRegion, nSite = nSite, beta.vec = beta.vec, x = x, region = region, 
    wetness = wetness, N = N, C = C))
}


#' Simulate data for Chapter 17: Binomial GLMM
#'
#' Simulate Number of successful pairs ~ precipitation regressions in 16 populations of woodchat shrikes
#'
#' @param nPops Number of populations
#' @param nYears Number of years sampled in each population
#' @param mu.alpha Mean of random intercepts
#' @param sigma.alpha SD of random intercepts
#' @param mu.beta Mean of random slopes
#' @param sigma.beta SD of random slopes
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nPops}{Number of populations}
#'   \item{nYears}{Number of years sampled}
#'   \item{mu.alpha}{Mean of random intercepts}
#'   \item{sigma.alpha}{SD of random intercepts}
#'   \item{mu.beta}{Mean of random slopes}
#'   \item{sigma.beta}{SD of random slopes}
#'   \item{pop}{Population index}
#'   \item{precip}{Precipitation covariate values}
#'   \item{alpha}{Random intercepts}
#'   \item{beta}{Random slopes}
#'   \item{N}{Number of shrike pairs at each site}
#'   \item{C}{Number of successful shrike pairs at each site}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' library(lattice)
#' str(dat <- simDat17())      # Implicit default arguments (DOES NOT PRODUCE PLOT FOR SOME REASON)
#' xyplot(dat$C/dat$N ~ dat$precip | dat$pop, ylab = "Realized woodchat shrike breeding success ", 
#'    xlab = "Spring precipitation index", main = "Realized breeding success", pch = 16, cex = 1.2,
#'	col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to random intercept model. Increased sigma.alpha to emphasize the random intercepts part
#' str(dat <- simDat17(nPops = 16, sigma.alpha = 1, sigma.beta = 0))
#' xyplot(dat$C/dat$N ~ dat$precip | dat$pop, ylab = "Realized woodchat shrike breeding success ", 
#'    xlab = "Spring precipitation index", main = "Realized breeding success (random-intercepts model)",
#'	pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to random-effects one-way binomial ANOVA model: random intercepts, but zero slopes
#' str(dat <- simDat17(nPops = 16, sigma.alpha = 1, mu.beta = 0, sigma.beta = 0))
#' xyplot(dat$C/dat$N ~ dat$precip | dat$pop, ylab = "Realized woodchat shrike breeding success ", 
#'     xlab = "Spring precipitation index",
#' main = "Realized breeding success (random-effects,\none-way binomial ANOVA model)", 
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to simple log-linear binomial (i.e., logistic) regression
#' #   (= no effects of pop on either intercepts or slopes)
#' str(dat <- simDat17(nPops = 16, sigma.alpha = 0, sigma.beta = 0))
#' xyplot(dat$C/dat$N ~ dat$precip | dat$pop, ylab = "Realized woodchat shrike breeding success ", 
#'    xlab = "Spring precipitation index", 
#' main = "Realized breeding success\n(simple logistic regression model)", 
#' pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' # Revert to binomial "model-of-the-mean": no effects of either population or precipitation
#' str(dat <- simDat17(nPops = 16, sigma.alpha = 0, mu.beta = 0, sigma.beta = 0))
#' xyplot(dat$C/dat$N ~ dat$precip | dat$pop, ylab = "Realized woodchat shrike breeding success ", 
#'  xlab = "Spring precipitation index", 
#'  main = "Realized breeding success (binomial 'model-of-the-mean')",
#'	pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))
#'
#' @importFrom lattice xyplot
#' @importFrom grDevices rgb
#' @importFrom stats model.matrix rnorm rbinom runif
#' @export
simDat17 <- function(nPops = 16, nYears = 10, mu.alpha = 0, mu.beta = -2, sigma.alpha = 1, sigma.beta = 1){
  n <- nPops * nYears
  pop <- gl(n = nPops, k = nYears)
  precip <- runif(n, -1, 1)
  N <- round(runif(n, 20, 50) )    # Choose number of studied pairs
  Xmat <- model.matrix(~ pop * precip - 1 - precip)
  alpha <- rnorm(n = nPops, mean = mu.alpha, sd = sigma.alpha)
  beta <- rnorm(n = nPops, mean = mu.beta, sd = sigma.beta)
  all.pars <- c(alpha, beta)
  lin.pred <- Xmat %*% all.pars
  exp.p <- exp(lin.pred) / (1 + exp(lin.pred))
  C <- rbinom(n = n, size = N, prob = exp.p)
  xyplot(C/N ~ precip | pop, ylab = "Realized woodchat shrike breeding success ", 
    xlab = "Spring precipitation index", main = "Realized breeding success", pch = 16, cex = 1.2,
	col = rgb(0, 0, 0, 0.4))     ### %%%%%% DOES NOT PRODUCE THE PLOT
  return(list(nPops = nPops, nYears = nYears, mu.alpha = mu.alpha, mu.beta = mu.beta,
    sigma.alpha = sigma.alpha, sigma.beta = sigma.beta, pop = pop, precip = precip,   
	alpha = alpha, beta = beta, N = N, C = C))
}


#' Simulate data for Chapter 19: Site-occupancy model
#'
#' Simulate Detection/nondetection data of Chiltern gentians
#'
#' @param nSites Number of sites
#' @param nVisits Number of replicate visits per site
#' @param alpha.occ Occupancy intercept
#' @param beta.occ Occupancy slope
#' @param alpha.p Detection probability intercept
#' @param beta.p Detection probability slope
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nSites}{Number of sites}
#'   \item{nVisits}{Number replicate visits per site}
#'   \item{alpha.occ}{Occupancy intercept}
#'   \item{beta.occ}{Occupancy slope}
#'   \item{alpha.p}{Detection probability intercept}
#'   \item{beta.p}{Detection probability slope}
#'   \item{humidity}{Humidity covariate}
#'   \item{occ.prob}{Probability of occupancy at each site}
#'   \item{z}{True occupancy state at each site}
#'   \item{true_Nz}{True number of occupied sites}
#'   \item{lp}{Linear predictor for detection}
#'   \item{p}{Probability of detection at each site}
#'   \item{y}{Simulated detection/non-detection data}
#'   \item{obs_Nz}{Observed number of occupied sites}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat19())             # Implicit default arguments
#' str(dat <- simDat19(nSites = 150, nVisits = 3, alpha.occ = 0, beta.occ = 2,
#'   alpha.p = 0, beta.p = -3))       # Explicit default arguments
#' str(dat <- simDat19(nSites = 500)) # More sites
#' str(dat <- simDat19(nVisits = 1))  # Single-visit data
#' str(dat <- simDat19(nVisits = 20)) # 20 visits, will yield cumulative detection prob of about 1
#' str(dat <- simDat19(alpha.occ = 2))# Much higher occupancy
#' str(dat <- simDat19(beta.occ = 0)) # No effect of humidity on occupancy
#' str(dat <- simDat19(beta.p = 3))   # Positive effect of humidity on detection
#' str(dat <- simDat19(beta.p = 0))   # No effect of humidity on detection
#'
#' @importFrom lattice xyplot
#' @importFrom graphics par polygon
#' @importFrom grDevices rgb
#' @importFrom stats runif rbinom plogis glm predict binomial
#' @export
simDat19 <- function(nSites = 150, nVisits = 3, alpha.occ = 0, beta.occ = 2, alpha.p = 0, beta.p = -3){
  humidity <- runif(n = nSites, min = -1, max =1)
  occ.prob <- plogis(alpha.occ + beta.occ * humidity)
  z <- rbinom(n = nSites, size = 1, prob = occ.prob)
  true_Nz <- sum(z) # True number of occupied sites among the visited sites
  # Observation process
  lp <- alpha.p + beta.p * humidity    # The linear predictor for detection
  p <- plogis(lp)       # Get p on the probability scale
  y <- array(dim = c(nSites, nVisits))
  for(t in 1:nVisits){
    y[,t] <- rbinom(n = nSites, size = 1, prob = z * p)
  }
  obs_Nz <- sum(apply(y, 1, sum) > 0)  # Apparent number of occupied among visited sites
  obsZ <- as.numeric(apply(y, 1, sum) > 0)  # 'Observed presence/absence'
  naive.analysis <- glm(obsZ ~ humidity, family = binomial)
  summary(naive.analysis)
  lpred.naive <- predict(naive.analysis, type = 'link', se = TRUE)
  pred.naive <- plogis(lpred.naive$fit)
  LCL.naive <- plogis(lpred.naive$fit-2*lpred.naive$se)
  UCL.naive <- plogis(lpred.naive$fit+2*lpred.naive$se)

  par(mfrow = c(1, 3), mar = c(6,6,5,4), cex.lab = 1.6, cex.axis = 1.6, cex.main = 2)
  plot(humidity, occ.prob, ylim = c(0,1), xlab = "Humidity index", ylab = "Occupancy probability", main = "State process", las = 1, pch = 16, cex = 2, col = rgb(0,0,0,0.3), frame = FALSE)
  plot(humidity, p, ylim = c(0,1), xlab = "Humidity index", ylab = "Detection probability", main = "Observation process", las = 1, pch = 16, cex = 2, col = rgb(0,0,0,0.3), frame = FALSE)
  plot(humidity, pred.naive, ylim = c(0, max(UCL.naive)), xlab = "Humidity index", ylab = "Apparent occupancy prob.", main = "Confounding of\nstate and observation processes", las = 1, pch = 16, cex = 2, col = rgb(0,0,0,0.4), frame = FALSE)
  polygon(c(sort(humidity), rev(humidity[order(humidity)])), c(LCL.naive[order(humidity)], rev(UCL.naive[order(humidity)])), col = rgb(0,0,0, 0.2), border = NA)
  return(list(nSites = nSites, nVisits = nVisits, alpha.occ = alpha.occ, beta.occ = beta.occ,
  alpha.p = alpha.p, beta.p = beta.p, humidity = humidity, occ.prob = occ.prob, z = z, true_Nz = true_Nz,
  lp = lp, p = p, y = y, obs_Nz = obs_Nz))
}


#' Simulate data for Chapter 20: Integrated model
#'
#' Simulate three count datasets under different data collection conditions
#'
#' @param nsites1 Number of sites in regular count dataset
#' @param nsites2 Number of sites in zero-truncated count dataset
#' @param nsites3 Number of sites in detection/non-detection dataset
#' @param mean.lam Mean site abundance
#' @param beta Slope for elevation covariate
#'
#' @return A list of simulated data and parameters.
#' \itemize{
#'   \item{nsites1}{Number of sites in regular count dataset}
#'   \item{nsites2}{Number of sites in zero-truncated count dataset}
#'   \item{nsites3}{Number of sites in detection/non-detection dataset}
#'   \item{mean.lam}{Mean site abundance}
#'   \item{beta}{Slope for elevation covariate}
#'   \item{C1}{Simulated regular counts from dataset 1}
#'   \item{C2}{Simulated regular counts from dataset 2}
#'   \item{C3}{Simulated regular counts from dataset 3}
#'   \item{ztC2}{Simulated zero-truncated counts from dataset 2}
#'   \item{y}{Simulated detection/non-detection data from dataset 3}
#' }
#'
#' @author Marc Kéry
#'
#' @examples
#' str(dat <- simDat20())             # Implicit default arguments
#'
#' # Revert to an 'integrated Poisson/binomial model-of-the-mean': no effect of elevation on abundance
#' str(dat <- simDat20(nsites1 = 500, nsites2 = 1000, nsites3 = 2000, mean.lam = 2, beta = 0))
#'
#' @importFrom graphics par axis points
#' @importFrom grDevices adjustcolor
#' @importFrom stats runif rpois
#' @export
simDat20 <- function(nsites1 = 500, nsites2 = 1000, nsites3 = 2000, mean.lam = 2, beta = -2){
  # Simulate elevation covariate for all three and standardize 
  # to mean of 1000 and standard deviation also of 1000 m
  elev1 <- sort(runif(nsites1, 200, 2000))   # Imagine 200-2000 m a.s.l. 
  elev2 <- sort(runif(nsites2, 200, 2000))
  elev3 <- sort(runif(nsites3, 200, 2000))
  selev1 <- (elev1 - 1000) / 1000     # Scaled elev1
  selev2 <- (elev2 - 1000) / 1000     # Scaled elev2
  selev3 <- (elev3 - 1000) / 1000     # Scaled elev3
  # Create three regular count data sets with log-linear effects
  C1 <- rpois(nsites1, exp(log(mean.lam) + beta * selev1))
  C2 <- rpois(nsites2, exp(log(mean.lam) + beta * selev2))
  C3 <- rpois(nsites3, exp(log(mean.lam) + beta * selev3))
  # Create data set 2 (C2) by zero-truncating (discard all zeroes)
  ztC2 <- C2              # Make a copy
  ztC2 <- ztC2[ztC2 > 0]  # Tossing out zeroes yields zero-truncated data
  # Turn count data set 3 (C3) into detection/nondetection data (y)
  y <- C3                 # Make a copy
  y[y > 1] <- 1           # Squash to binary
  # Plot counts/DND data in all data sets (Fig. 20–3)
  par(mfrow = c(1, 2), mar = c(5, 5, 4, 1), cex = 1.2, cex.lab = 1.5, cex.axis = 1.5, las = 1)
  plot(elev2[C2>0], jitter(ztC2), pch = 16, xlab = 'Elevation (m)', ylab = 'Counts', frame = FALSE, ylim = range(c(C1, ztC2)), col = adjustcolor('grey80', 1), main = 'Data sets 1 and 2')
  points(elev1, jitter(C1), pch = 16)
  # lines(200:2000, exp(log(2) -2 * ((200:2000)-1000)/1000 ), col = 'red', lty = 1, lwd = 2) # Perhaps adapt
  axis(1, at = c(250, 750, 1250, 1750), tcl = -0.25, labels = NA)
  plot(elev3, jitter(y, amount = 0.04), xlab = 'Elevation (m)', ylab = 'Detection/nondetection', axes = FALSE, pch = 16, col = adjustcolor('grey60', 0.3), main = 'Data set 3')
  axis(1)
  axis(1, at = c(250, 750, 1250, 1750), tcl = -0.25, labels = NA)
  axis(2, at = c(0, 1), labels = c(0, 1))
  return(list(nsites1 = nsites1, nsites2 = nsites2, nsites3 = nsites3, mean.lam = mean.lam, beta = beta,
  C1 = C1, C2 = C2, C3 = C3, ztC2 = ztC2, y = y))
}
