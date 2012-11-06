\name{plot.nparcomp}
\alias{plot.nparcomp}

\title{
Visualizing the result of \code{\link{nparcomp}}
}
\description{
This function takes an object of class "nparcomp" and creates a plot of the
confidence intervals for the estimated nonparametric contrast effects.
}
\usage{
\method{plot}{nparcomp}(x,...)
}

\arguments{
  \item{x}{An object of class "nparcomp", i.e. the result when 
                    applying \code{\link{nparcomp}} to a dataset. Otherwise an 
                    error will occur.
}
  \item{...}{Arguments to be passed to methods.
}
}
\details{
It is not possible to change any parameter set in 
the \code{\link{nparcomp}}-statement.

Since plot.nparcomp is a S3 method it suffices to use plot(x) as long as x is of 
class "nparcomp". It will be interpreted as plot.nparcomp(x). 
}
\value{
plot.nparcomp returns a graph that contains a confidence interval for the estimated
nonparametric contrast effect of each contrast. It just visualizes the result 
of the \code{\link{nparcomp}}-statement.
}
\references{
 Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample Approximations.
 
 Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43, 553-569.
}
\author{
Frank Konietschke
}
\note{
It is possible to create a graphical result directly
by setting plot.simci=TRUE in the \code{\link{nparcomp}}-statement. 
}


\seealso{
For further information on the usage of nparcomp, see \code{\link{nparcomp}}.
}
\examples{
data(liver)
a<-nparcomp(weight ~dosage, data=liver, asy.method = "probit", 
            type = "Williams", alternative = "two.sided", 
            plot.simci = FALSE, info = FALSE)
plot(a)
}

\keyword{aplot}

