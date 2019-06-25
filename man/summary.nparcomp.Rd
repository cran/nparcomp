\name{summary.nparcomp}
\alias{summary.nparcomp}

\title{
Summary of \code{\link{nparcomp}}
}
\description{
The function summary.nparcomp produces a result summary of \code{\link{nparcomp}}. It
can only be applied to objects of class "nparcomp".
}
\usage{
\method{summary}{nparcomp}(object,...)
}

\arguments{
  \item{object}{An object of class "nparcomp", i.e. the result when 
                    applying \code{\link{nparcomp}} to a dataset. Otherwise an 
                    error will occur.
}
  \item{...}{Arguments to be passed to methods.
}
}
\details{
Since summary.nparcomp is a S3 method it suffices to use summary(x) as long as x is of 
class "nparcomp". It will be interpreted as summary.nparcomp(x).
}
\value{
The function produces a summary of the result of \code{\link{nparcomp}} starting 
with some global information: alternative hypothesis, estimation method, type of
contrast, confidence level, method, interpretation. This is followed by:
\item{Data.Info }{List of samples and sample sizes. }
   \item{Contrast}{Contrast matrix.}
   \item{Analysis }{Comparison: relative contrast effect ,
                           relative.effect: estimated relative contrast effect,
                           Estimator: Estimated relative contrast effect,
                           Lower: Lower limit of the simultaneous confidence interval,
                           Upper: Upper limit of the simultaneous confidence interval,
                           Statistic: Teststatistic
                           p.Value: Adjusted p-values for the hypothesis by the choosen approximation method.
                          }
   \item{Overall}{Overall p-value and critical value.}
}
\references{
Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample Approximations.

Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43, 553-569.  
}
\author{
Frank Konietschke
}
\note{
It is possible to create a graphical result of the nonparametric test procedure nparcomp
by using the function \code{\link{plot.nparcomp}}.
}

\seealso{
For further information on the usage of nparcomp, see \code{\link{nparcomp}}.
}

\examples{
\dontrun{
data(liver)
a<-nparcomp(weight ~dosage, data=liver, asy.method = "probit", 
            type = "Williams", alternative = "two.sided", 
            plot.simci = FALSE, info = FALSE)
summary(a)
}}

\keyword{print}

