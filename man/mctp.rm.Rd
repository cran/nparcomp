\name{mctp.rm}
\alias{mctp.rm}

\title{
MCTP and SCIs in a repeated measures design (one group)
}
\description{
In the setting of a repeated measures design with n independent individuals and
d repeated measures the function mctp.rm computes the estimator of nonparametric relative effects based on global rankings. 
Simultaneous confidence intervals for the effects and adjusted p-values based on special contrasts like "UserDefined", "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint",
 "AVE", "McDermott", "Marcus", "UmbrellaWilliams" are provided. The statistics are computed using multivariate normal distribution, multivariate Satterthwaite t-Approximation
and multivariate transformations (Fisher function).
The function 'mctp.rm' also computes one-sided and two-sided confidence intervals and p-values. The confidence intervals can be plotted.
}
\usage{
mctp.rm(formula, data, type = c("UserDefined", "Tukey", "Dunnett", 
        "Sequen", "Williams", "Changepoint", "AVE", "McDermott", 
        "Marcus", "UmbrellaWilliams"), control = NULL, conf.level = 0.95, 
        alternative = c("two.sided", "lower", "greater"), rounds = 3, 
        correlation = FALSE, 
        asy.method = c("fisher", "normal", "mult.t"), 
        plot.simci = FALSE, info = TRUE, contrast.matrix = NULL)
}

\arguments{
  \item{formula}{A two-sided 'formula' specifying a numeric response variable
          and a repeated measures factor with more than two levels. 
          If the factor contains less than 3 levels, an error message will be returned.
}
  \item{data}{A dataframe containing the variables specified in formula.
}
  \item{type}{Character string defining the type of contrast. It should be one of  "UserDefined",
  "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus", "UmbrellaWilliams".
}
  \item{control}{If type=Dunnett, specification of the factor code which should serve as control (first level is default).
}
  \item{conf.level}{The confidence level for \code{conflevel}-confidence intervals (default is 0.95).
}
  \item{alternative}{Character string defining the alternative hypothesis, one
          of "two.sided", "less" or "greater".
}
  \item{rounds}{Number of rounds for the numeric values of the output (default is 3).
}
  \item{correlation}{A logical whether the estimated correlation matrix and covariance matrix should be printed.
}
  \item{asy.method}{Character string defining the asymptotic approximation method, one
          of "mult.t" for using a multivariate t-distribution with a Satterthwaite Approximation, "fisher" for using
          the Fisher transformation function, "normal",
          for using the multivariate normal distribution.
}
  \item{plot.simci}{A logical indicating whether you want a plot of the confidence intervals.
}
  \item{info}{A logical whether you want a brief overview with informations about the output.
}
  \item{contrast.matrix}{User defined contrast matrix.
}
}

\value{

   \item{Data.Info }{List of samples and sample sizes and estimated effect per repeated measures level.}
   \item{Contrast}{Contrast matrix.}
   \item{Analysis }{       Estimator: Estimated relative effect,
                           Lower: Lower limit of the simultaneous confidence interval,
                           Upper: Upper limit of the simultaneous confidence interval,
                           Statistic: Teststatistic
                           p.Value: Adjusted p-values for the hypothesis by the choosen approximation method.
                          }
   \item{ input }{List of input by user.}
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
Estimated relative effects with 0 or 1 are replaced with 0.001 and 0.999.

A summary and a graph can be created separately by using the functions
\code{\link{summary.mctp.rm}} and \code{\link{plot.mctp.rm}}.

For the analysis, the R packages 'multcomp' and 'mvtnorm' are required.
}

\seealso{
To analyse simple one-way layouts with independent samples use \code{\link{mctp}}. 
}
\examples{
data(panic)
a<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
           alternative = "two.sided",
           asy.method = "mult.t", plot.simci = FALSE,
           info = FALSE, contrast.matrix = NULL)
summary(a)
plot(a)

mctp.rm(CGI~week, data=panic, type = "Tukey",
        alternative = "two.sided",
        asy.method = "mult.t", plot.simci = TRUE)
}
\keyword{htest}

\concept{ Nonparametric }
\concept{ Contrast }
\concept{ Relative Effect }
\concept{ Simultaneous Confidene Intervals }
\concept{ Repeated Measures }