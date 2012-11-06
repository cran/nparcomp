\name{summary.mctp}
\alias{summary.mctp}

\title{
Summary of \code{\link{mctp}}
}
\description{
The function summary.mctp produces a result summary of \code{\link{mctp}}. It
can only be applied to objects of class "mctp".
}
\usage{
\method{summary}{mctp}(object,...)
}

\arguments{
  \item{object}{An object of class "mctp", i.e. the result when 
                    applying \code{\link{mctp}} to a dataset. Otherwise an 
                    error will occur.
}
  \item{...}{Arguments to be passed to methods.
}
}
\details{
Since summary.mctp is a S3 method it suffices to use summary(x) as long as x is of 
class "mctp". It will be interpreted as summary.mctp(x). 
}
\value{
The function produces a summary of the result of \code{\link{mctp}} starting 
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
F. Konietschke, L.A. Hothorn, E. Brunner: Rank-Based Multiple Test Procedures and
Simultaneous Confidence Intervals. Electronic Journal of Statistics, Vol.0 (2011) 1-8.
}
\author{
Frank Konietschke
}
\note{
It is possible to create a graphical result of the multiple comparison test
procedure by using the function \code{\link{plot.mctp}}. 
}
\seealso{
For further information on the usage of mctp, see \code{\link{mctp}}.
}
\examples{
data(liver)
a<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
        type = "Dunnett", alternative = "two.sided", plot.simci = FALSE, 
        info = FALSE)
summary(a)
}

\keyword{print}
