% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataSimulation.R
\name{simDat102}
\alias{simDat102}
\title{Simulate data for Chapter 10.2: Linear mixed-effects model}
\usage{
simDat102(
  nPops = 56,
  nSample = 10,
  mu.alpha = 260,
  sigma.alpha = 20,
  mu.beta = 60,
  sigma.beta = 30,
  sigma = 30
)
}
\arguments{
\item{nPops}{Number of populations}

\item{nSample}{Samples from each population}

\item{mu.alpha}{Mean of random intercepts}

\item{sigma.alpha}{SD of random intercepts}

\item{mu.beta}{Mean of random slopes}

\item{sigma.beta}{SD of random slopes}

\item{sigma}{Residual standard deviation}
}
\value{
A list of simulated data and parameters.
  \item{nPops}{Number of populations}
  \item{nSample}{Number of samples per population}
  \item{mu.alpha}{Mean of random intercepts}
  \item{sigma.alpha}{SD of random intercepts}
  \item{mu.beta}{Mean of random slopes}
  \item{sigma.beta}{SD of random slopes}
  \item{sigma}{Residual SD}
  \item{pop}{Indicator for population number}
  \item{orig.length}{Snake body length, not standardized}
  \item{lengthN}{Snake body length, standardized}
  \item{alpha}{Random intercepts}
  \item{beta}{Random slopes}
  \item{eps}{Residuals}
  \item{mass}{Simulated body mass for each snake}
}
\description{
Simulate mass ~ length regressions in 56 populations of snakes
with random population effects for intercepts and slopes.
There is no correlation between the intercept and slope random variables.
}
\examples{
library(lattice)
str(dat <- simDat102())      # Implicit default arguments
xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
       main = 'Realized mass-length relationships', pch = 16, cex = 1.2, 
       col = rgb(0, 0, 0, 0.4))

# Fewer populations, more snakes (makes patterns perhaps easier to see ?)
str(dat <- simDat102(nPops = 16, nSample = 100))
xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
       main = 'Realized mass-length relationships
       (default random-coefficients model)', 
       pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))

# Revert to random intercept model (and less residual variation), fewer pops 
# and more snakes. Increased sigma.alpha to emphasize the random intercepts part
str(dat <- simDat102(nPops = 16, nSample = 100, sigma.alpha = 50, sigma.beta = 0, sigma = 10))
xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
       main = 'Realized mass-length relationships (random-intercepts model)', 
       pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))

# Revert to random-effects one-way ANOVA model, only random intercepts, but zero slopes
str(dat <- simDat102(nPops = 16, nSample = 100, sigma.alpha = 50, 
                     mu.beta = 0, sigma.beta = 0, sigma = 10))
xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
       main = 'Realized mass-length relationships
       (one-way ANOVA model with random pop effects)', 
       pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))

# Revert to simple linear regression (= no effects of pop on either intercepts or slopes)
str(dat <- simDat102(nPops = 16, nSample = 100, sigma.alpha = 0, sigma.beta = 0, sigma = 10))
xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
       main = 'Realized mass-length relationships
       (de-facto a simple linear regression now)', 
       pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))

# Revert to "model-of-the-mean": no effects of either population or body length
str(dat <- simDat102(nPops = 16, nSample = 100, sigma.alpha = 0, mu.beta = 0, 
                     sigma.beta = 0, sigma = 10))
xyplot(dat$mass ~ dat$lengthN | dat$pop, xlab = 'Length', ylab = 'Mass', 
       main = 'Realized mass-length relationships
       ("model-of-the-mean", no effects of pop or length)', 
       pch = 16, cex = 1.2, col = rgb(0, 0, 0, 0.4))

}
\author{
Marc Kéry
}
