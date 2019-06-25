\name{plot.mctp.rm}
\alias{plot.mctp.rm}

\title{
Visualizing the result of \code{\link{mctp.rm}}
}
\description{
This function takes an object of class "mctp.rm" and creates a plot of the
confidence intervals for the estimated effects.
}
\usage{
\method{plot}{mctp.rm}(x,...)
}        

\arguments{
  \item{x}{An object of class "mctp.rm", i.e. the result when 
                    applying \code{\link{mctp.rm}} to a dataset. Otherwise an 
                    error will occur.
}
  \item{...}{Arguments to be passed to methods.
}
}
\details{
It is not possible to change any parameter set in 
the \code{\link{mctp.rm}}-statement.

Since plot.mctp.rm is a S3 method it suffices to use plot(x) as long as x is of 
class "mctp.rm". It will be interpreted as plot.mctp.rm(x). 
}
\value{
plot.mctp.rm returns a graph that contains a confidence interval for the estimated
effect of each contrast. It just visualizes the result 
of the \code{\link{mctp.rm}}-statement.
}
\references{
F. Konietschke, A.C. Bathke, L.A. Hothorn, E. Brunner: Testing and estimation of 
purely nonparametric effects in repeated measures designs. Computational Statistics
and Data Analysis 54 (2010) 1895-1905.
}
\author{
Marius Placzek, Kimihiro Noguchi
}
\note{
It is possible to create a graphical result of the multiple comparison test
procedure directly by setting plot.simci=TRUE in the \code{\link{mctp.rm}}-statement.

To get a complete result summary of \code{\link{mctp.rm}} the function
\code{\link{summary.mctp.rm}} can be used.
}
\seealso{
For further information on the usage of mctp.rm, see \code{\link{mctp.rm}}.
}
\examples{
\dontrun{
data(panic)
a<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
           alternative = "two.sided",
           asy.method =  "fisher", contrast.matrix = NULL)
plot(a)
}}

\keyword{aplot}