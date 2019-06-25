\name{mctp.rm}
\alias{mctp.rm}

\title{
Nonparametric multiple contrast tests and simultaneous confidence intervals (repeated measures)
}
\description{
The function mctp.rm computes the estimator of nonparametric relative effects based on global rankings, simultaneous confidence intervals for the effects, and adjusted p-values based on contrasts in the setting of a repeated measures design with n independent individuals and d repeated measures. Contrasts include "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus", "UmbrellaWilliams", "GrandMean", and "UserDefined". The statistics are computed using multivariate normal distribution, multivariate Satterthwaite t-Approximation, and multivariate transformations (adjusted log odds or Fisher function). The function 'mctp.rm' computes both the one-sided and two-sided simultaneous confidence intervals and adjusted p-values. The confidence intervals can be plotted.
}
\usage{
mctp.rm(formula, data, type = c("Tukey", "Dunnett", "Sequen", 
     "Williams", "Changepoint", "AVE", "McDermott", "Marcus", 
     "UmbrellaWilliams", "GrandMean", "UserDefined"), 
     conf.level = 0.95, alternative = c("two.sided", "less", 
     "greater"), asy.method = c("log.odds", "fisher", "mult.t", 
     "normal"), plot.simci = FALSE, control = NULL, info = TRUE, 
     rounds = 3, contrast.matrix = NULL, correlation = FALSE, 
     const=1/1.702)        
}

\arguments{
  \item{formula}{A two-sided 'formula' specifying a numeric response variable
          and a repeated measures factor with more than two levels. 
          If the factor contains less than 3 levels, an error message will be returned.
}
  \item{data}{A dataframe containing the variables specified in formula.
}
  \item{type}{Character string defining the type of contrast. It should be one of 
  "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus", "UmbrellaWilliams", "GrandMean", 
  "UserDefined".  	
}
  \item{conf.level}{The confidence level for \code{conf.level}-confidence intervals (default is 0.95).
}
  \item{alternative}{Character string defining the alternative hypothesis, one
          of "two.sided", "less", or "greater".
}
  \item{asy.method}{Character string defining the asymptotic approximation method, one of
		  "log.odds" (for using the adjusted log odds effect sizes), 
          "mult.t" (for using a multivariate t-distribution with a Satterthwaite Approximation), 
          "fisher" (for using the Fisher transformation function), or 
          "normal" (for using the multivariate normal distribution).
}
  \item{plot.simci}{A logical indicating whether you want a plot of the confidence intervals.
}
  \item{control}{Character string defining the control group in Dunnett comparisons. By default, it is the first group by
 definition of the factor variable.
}
  \item{info}{A logical whether you want a brief overview with informations about the output.
}
  \item{rounds}{Number of rounds for the numeric values of the output (default is 3).
}
  \item{contrast.matrix}{User-defined contrast matrix.
}
  \item{correlation}{A logical whether the estimated correlation matrix and covariance matrix should be printed.
}
  \item{const}{Number used for the adjustment of log odds when the "log.odds" option is chosen.
}
}

\value{

   \item{Data.Info}{List of samples and sample sizes and estimated effect per repeated measures level.}
   \item{Contrast}{Contrast matrix.}
   \item{Analysis}{       Estimator: Estimated relative effect,
                           Lower: Lower limit of the simultaneous confidence intervals,
                           Upper: Upper limit of the simultaneous confidence intervals,
                           Statistic: Test statistic
                           p.Value: Adjusted p-values for the hypothesis by the choosen approximation method.
                          }
   \item{Analysis.Inf}{The same as \code{Analysis} except that it assumes \code{rounds = Inf}.}    
   \item{Overall}{The critical value and adjusted p-value for the overall hypothesis.}                          
   \item{input}{List of input arguments by user.}
   \item{text.Output}{Character string specifying the alternative hypotheses.}   
   \item{connames}{Character string specifying the contrast names.}
   \item{AsyMethod}{Character string specifying the approximation method.}        
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
Estimated relative effects with 0 or 1 are replaced with 0.001 and 0.999.

A summary and a graph can be created separately by using the functions
\code{\link{summary.mctp.rm}} and \code{\link{plot.mctp.rm}}.

For the analysis, the R packages 'multcomp' and 'mvtnorm' are required.
}

\seealso{
To analyse simple one-way layouts with independent samples use \code{\link{mctp}}. 
}
\examples{
\dontrun{
data(panic)
a<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
           alternative = "two.sided",
           asy.method = "log.odds", plot.simci = FALSE,
           info = FALSE, contrast.matrix = NULL)
summary(a)
plot(a)

b<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
           alternative = "two.sided",
           asy.method = "mult.t", plot.simci = FALSE,
           info = FALSE, contrast.matrix = NULL)
summary(b)
plot(b)

c<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
           alternative = "two.sided",
           asy.method = "fisher", plot.simci = FALSE,
           info = FALSE, contrast.matrix = NULL)
summary(c)
plot(c)

d<-mctp.rm(CGI~week, data=panic, type = "Tukey",
        alternative = "two.sided",
        asy.method = "mult.t", plot.simci = TRUE)
summary(d)
}}
\keyword{htest}

\concept{ Nonparametric }
\concept{ Contrast }
\concept{ Relative Effect }
\concept{ Simultaneous Confidene Intervals }
\concept{ Repeated Measures }