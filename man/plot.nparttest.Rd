\name{plot.nparttest}
\alias{plot.nparttest}

\title{
Visualizing the result of \code{\link{npar.t.test}}
}
\description{
This function takes an object of class "nparttest" and creates a plot of the
confidence interval for the estimated effect.
}
\usage{
\method{plot}{nparttest}(x,...)
}
\arguments{
  \item{x}{\code{x} An object of class "nparttest", i.e. the result when 
                    applying \code{\link{npar.t.test}} to a dataset. Otherwise an 
                    error will occur.
}
  \item{...}{\code{...} Arguments to be passed to methods.
}
}
\details{
It is not possible to change any parameter set in 
the \code{\link{npar.t.test}}-statement.

Since plot.nparttest is a S3 method it suffices to use plot(x) as long as x is of 
class "nparttest". It will be interpreted as plot.nparttest(x).
}
\value{
plot.npar.t.test returns a graph that contains a confidence interval for the estimated
effect of the nonparametric t-test. It just visualizes the result 
of the \code{\link{npar.t.test}}-statement.
}
\references{
Brunner, E., Munzel, U. (2000). The Nonparametric Behrens-Fisher Problem: Asymptotic Theory and a Small Sample Approximation. Biometrical Journal 42, 17 -25.

Neubert, K., Brunner, E., (2006). A Studentized Permutation Test for the Nonparametric Behrens-Fisher Problem. Computational Statistics and Data Analysis.
}
\author{
Frank Konietschke
}
\note{
It is possible to create a graphical result of the nonparametric t-test directly 
by setting plot.simci=TRUE in the \code{\link{npar.t.test}}-statement. 
}


\seealso{
For further information on the usage of npar.t.test, see \code{\link{npar.t.test}}.
}
\examples{
\dontrun{
data(impla)
a<-npar.t.test(impla~group, data = impla, method = "t.app",
               alternative = "two.sided", plot.simci=FALSE)
plot(a)
}}

\keyword{aplot}