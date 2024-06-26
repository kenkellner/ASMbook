% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataSimulation.R
\name{simDat11}
\alias{simDat11}
\title{Simulate data for Chapter 11: Comparing two groups of Poisson counts}
\usage{
simDat11(nSites = 30, alpha = log(2), beta = log(5) - log(2))
}
\arguments{
\item{nSites}{Number of sites}

\item{alpha}{Intercept}

\item{beta}{Slope for land use}
}
\value{
A list of simulated data and parameters.
  \item{nSites}{Number of sites}
  \item{alpha}{Intercept}
  \item{beta}{Slope for land use}
  \item{y}{Simulated hare counts}
}
\description{
Generate counts of hares in two areas with different landuse
}
\examples{
str(dat <- simDat11())      # Implicit default arguments

# Revert to "Poisson model-of-the-mean" 
# (Increase sample size to reduce sampling variability)
str(dat <- simDat11(nSites = 1000, beta = 0)) 

}
\author{
Marc Kéry
}
