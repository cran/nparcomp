\name{plot.mctp}
\alias{plot.mctp}

\title{
Visualizing the result of \code{\link{mctp}}
}
\description{
This function takes an object of class "mctp" and creates a plot of the
confidence intervals for the estimated effects.
}
\usage{
\method{plot}{mctp}(x,...)
}        

\arguments{
  \item{x}{An object of class "mctp", i.e. the result when 
                    applying \code{\link{mctp}} to a dataset. Otherwise an 
                    error will occur.
}
  \item{...}{Arguments to be passed to methods.
}
}
\details{
It is not possible to change any parameter set in 
the \code{\link{mctp}}-statement.

Since plot.mctp is a S3 method it suffices to use plot(x) as long as x is of 
class "mctp". It will be interpreted as plot.mctp(x). 
}
\value{
plot.mctp returns a graph that contains a confidence interval for the estimated
effect of each contrast. It just visualizes the result 
of the \code{\link{mctp}}-statement.
}
\references{
F. Konietschke, L.A. Hothorn, E. Brunner: Rank-Based Multiple Test Procedures and
Simultaneous Confidence Intervals. Electronic Journal of Statistics, Vol.0 (2011) 1-8.
}
\author{
Frank Konietschke, Kimihiro Noguchi
}
\note{
It is possible to create a graphical result of the multiple comparison test
procedure directly by setting plot.simci=TRUE in the \code{\link{mctp}}-statement.

To get a complete result summary of \code{\link{mctp}} the function
\code{\link{summary.mctp}} can be used.
}
\seealso{
For further information on the usage of mctp, see \code{\link{mctp}}.
}
\examples{
data(liver)
a<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
        type = "Dunnett", alternative = "two.sided", plot.simci = FALSE, 
        info = FALSE)
plot(a)
}

\keyword{aplot}