\name{weight.matrix}
\alias{weight.matrix}

\title{ Nonparametric relative contrast effects: the weight matrix }
\description{
 The function weight.matrix computes the weight matrix for a given contrast with which the estimator for the relative contrast
effect is comupted
}
\usage{
weight.matrix(n, type = c("UserDefined", "Tukey", "AVE",
 "Dunnett", "Sequen", "Changepoint", "Marcus", "McDermott",
 "Williams", "UmbrellaWilliams"), base = 1, contrast.matrix = NULL)
}

\arguments{
  \item{n}{  \code{n} vector of sample sizes }
  \item{type}{  \code{type} Character string defining the type of contrast. It should be one of
  "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus"  }
  \item{base}{  \code{base} Number of the basline values in Dunnett Comparisons. By default it is base = 1 }
  \item{contrast.matrix}{ optional, a user-defined \code{contrast.matrix} ... }
}
\details{
  The function needs the R-function 'contrMat' from the R-package 'multcomp'.
}
\value{
  The function returns the weight matrix which can be used to compute the estimator of the relative contrast effect
}
\references{
 Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample Approximations,
 Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Simultaneous Confidence Intervals for Relative Effects in Dunnett Comparsisons. }
\author{ Frank Konietschke }
\note{ For the analysis, the R-package 'multcomp' is required.
}
\seealso{ \code{contrMat} in package \pkg{multcomp} for contrast matrices}
\examples{
n<-c(10,20,30,40)
weight.matrix(n, "Tukey") # result: identity matrix by construction
weight.matrix(n, "Dunnett", base = 3) # Weight matrix for Dunnett comparison - 3rd group is baseline
weight.matrix(n, "Changepoint") # weight matrix for relative changepoint  effect

# For comparison, see the contrasts matrices 
# available in the multcomp package:

library(multcomp)
n<-c(10,10,20,30,40)
contrMat(n, "Changepoint") # Contrast matrix for Changepoint comparisons

}
\keyword{ htest }
\concept{ Nonparametric }
\concept{ Contrast}
\concept{ Relative Effect}
\concept{ Relative contrast effects}