% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataSimulation.R
\name{simDat16}
\alias{simDat16}
\title{Simulate data for Chapter 16: Binomial ANCOVA}
\usage{
simDat16(nRegion = 3, nSite = 10, beta.vec = c(-4, 1, 2, 6, 2, -5))
}
\arguments{
\item{nRegion}{Number of regions}

\item{nSite}{Number of sites per region}

\item{beta.vec}{Vector of regression coefficients}
}
\value{
A list of simulated data and parameters.
  \item{nRegion}{Number of regions}
  \item{nSite}{Number of sites per region}
  \item{beta}{Vector of regression coefficients}
  \item{x}{Indicator for region number}
  \item{region}{Region name (factor)}
  \item{wetness}{Wetness covariate}
  \item{N}{Number of adders captured at each site}
  \item{C}{Number of black adders captured at each site}
}
\description{
Simulate Number black individuals ~ wetness regressions in adders in 3 regions
}
\examples{
str(dat <- simDat16())      # Implicit default arguments

# Revert to main-effects model with parallel lines on the logit link scale
# (also larger sample size to better see patterns)
str(dat <- simDat16(nSite = 100, beta.vec = c(-4, 1, 2, 6, 0, 0)))

# Same with less strong logistic regression coefficient
str(dat <- simDat16(nSite = 100, beta.vec = c(-4, 1, 2, 3, 0, 0)))

# Revert to simple logit-linear binomial regression: no effect of pop (and weaker coefficient)
str(dat <- simDat16(nSite = 100, beta.vec = c(-4, 0, 0, 3, 0, 0)))

# Revert to one-way ANOVA binomial model: no effect of wetness
# (Choose greater differences in the intercepts to better show patterns)
str(dat <- simDat16(nSite = 100, beta.vec = c(-2, 2, 3, 0, 0, 0)))

# Revert to binomial "model-of-the-mean": no effects of either wetness or population
# Intercept chosen such that average proportion of black adders is 0.6
str(dat <- simDat16(nSite = 100, beta.vec = c(qlogis(0.6), 0, 0, 0, 0, 0)))
mean(dat$C / dat$N)        # Average is about 0.6

}
\author{
Marc Kéry
}
