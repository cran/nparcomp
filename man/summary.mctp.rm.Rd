\name{summary.mctp.rm}
\alias{summary.mctp.rm}

\title{
Summary of \code{\link{mctp.rm}}
}
\description{
The function summary.mctp.rm produces a result summary of \code{\link{mctp.rm}}. It
can only be applied to objects of class "mctp.rm".
}
\usage{
\method{summary}{mctp.rm}(object,...)
}

\arguments{
  \item{object}{An object of class "mctp.rm", i.e. the result when 
                    applying \code{\link{mctp.rm}} to a dataset. Otherwise an 
                    error will occur.
}
  \item{...}{Arguments to be passed to methods.
}
}
\details{
Since summary.mctp.rm is a S3 method it suffices to use summary(x) as long as x is of 
class "mctp.rm". It will be interpreted as summary.mctp.rm(x). 
}
\value{
The function produces a summary of the result of \code{\link{mctp.rm}} starting 
with some global information: alternative hypothesis, estimation method, type of
contrast, confidence level. This is followed by:
\item{Data.Info }{List of samples and sample sizes and estimated effect per group.}
\item{Contrast}{Contrast matrix.}
\item{Analysis }{Estimator: Estimated relative effect,
                 Lower: Lower limit of the simultaneous confidence interval,
                 Upper: Upper limit of the simultaneous confidence interval,
                 Statistic: Teststatistic
                 p.Value: Adjusted p-values for the hypothesis by the choosen approximation method.
                 } 
}
\references{
F. Konietschke, A.C. Bathke, L.A. Hothorn, E. Brunner: Testing and estimation of 
purely nonparametric effects in repeated measures designs. Computational Statistics
and Data Analysis 54 (2010) 1895-1905.
}
\author{
Marius Placzek
}
\note{
It is possible to create a graphical result of the multiple comparison test
procedure by using the function \code{\link{plot.mctp.rm}}. 
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
summary(a)
}}

\keyword{print}
