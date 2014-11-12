\name{plot.nparttestpaired}
\alias{plot.nparttestpaired}

\title{
Visualizing the result of \code{\link{npar.t.test.paired}}
}
\description{
This function takes an object of class "nparttestpaired" and creates a plot of the
confidence intervals for the estimated effect resulting from 
the studentized permutation test and the Brunner-Munzel test.
}
\usage{
\method{plot}{nparttestpaired}(x,...)
}
\arguments{
  \item{x}{\code{x} An object of class "nparttestpaired", i.e. the result when 
                    applying \code{\link{npar.t.test.paired}} to a dataset. Otherwise an 
                    error will occur.
}
  \item{...}{\code{...} Arguments to be passed to methods.
}
}
\details{
It is not possible to change any parameter set in 
the \code{\link{npar.t.test.paired}}-statement.

Since plot.nparttestpaired is a S3 method it suffices to use plot(x) as long as x is of 
class "nparttestpaired". It will be interpreted as plot.nparttestpaired(x).
}
\value{
plot.npar.t.test returns a graph that contains a confidence interval for the estimated
effect of the nonparametric studentized permutation test as well as. It just visualizes the result 
of the \code{\link{npar.t.test.paired}}-statement.
}
\references{
Munzel, U., Brunner, E. (2002). An Exact Paired Rank Test. Biometrical Journal 44, 584-593.

Konietschke, F., Pauly, M. (2012). A Studentized Permutation Test for the Nonparametric Behrens-Fisher Problem in Paired Data. Electronic Journal of Statistic, Vol 6, 1358-1372.
}
\author{
Frank Konietschke
}
\note{
It is possible to create a graphical result of the nonparametric studentized permutation test directly 
by setting plot.simci=TRUE in the \code{\link{npar.t.test.paired}}-statement. 
}


\seealso{
For further information on the usage of npar.t.test.paired, see \code{\link{npar.t.test.paired}}.
}
\examples{
data(PGI)
a<-npar.t.test.paired(PGIscore~timepoint, data = PGI, 
               alternative = "two.sided", info=TRUE, plot.simci=FALSE)
plot(a)
}

\keyword{aplot}