\name{mctp}
\alias{mctp}

\title{Nonparam. multiple contrast tests and simult. confidence intervals}
\description{
The function mctp computes the estimator of nonparametric relative effects based on global rankings, simultaneous confidence intervals for
 the effects and adjusted p-values based on special contrasts like "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint",
 "AVE", "McDermott", "Marcus", "UmbrellaWilliams", "UserDefined". The statistics are computed using multivariate normal distribution, multivariate Satterthwaite t-Approximation
 and multivariate transformations (Fisher function).
The function 'mctp' also computes one-sided and two-sided confidence intervals and p-values. The confidence intervals can be plotted.
}
\usage{
mctp(formula, data, type = c("Tukey", "Dunnett", "Sequen", 
     "Williams", "Changepoint", "AVE", "McDermott", "Marcus", 
     "UmbrellaWilliams", "UserDefined"), conf.level = 0.95, 
     alternative = c("two.sided", "less", "greater"), 
     asy.method = c("fisher", "mult.t", "normal"),
     plot.simci = FALSE, control = NULL, info = TRUE, rounds = 3, 
     contrast.matrix = NULL, correlation = FALSE, 
     effect=c("unweighted","weighted"))
}

\arguments{
  \item{formula}{A two-sided 'formula' specifying a numeric response variable
          and a factor with more than two levels. If the factor contains less than 3 levels, an error message will be returned.
}
  \item{data}{A dataframe containing the variables specified in formula.
}
  \item{type}{Character string defining the type of contrast. It should be one of 
  "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus", "UmbrellaWilliams", "UserDefined".
}
  \item{conf.level}{The confidence level for \code{conflevel}-confidence intervals (default is 0.95).
}
  \item{alternative}{Character string defining the alternative hypothesis, one
          of "two.sided", "less" or "greater".
}
  \item{asy.method}{Character string defining the asymptotic approximation method, one
          of "mult.t" for using a multivariate t-distribution with a Satterthwaite Approximation, "fisher" for using
          the Fisher transformation function, "normal",
          for using the multivariate normal distribution.
}
  \item{plot.simci}{A logical indicating whether you want a plot of the confidence intervals.
}
  \item{control}{Character string defining the control group in Dunnett comparisons. By default it is the first group by
 definition of the factor variable.
}
  \item{info}{A logical whether you want a brief overview with informations about the output.
}
  \item{rounds}{Number of rounds for the numeric values of the output (default is 3).
}
  \item{contrast.matrix}{User defined contrast matrix.
}
  \item{correlation}{A logical whether the estimated correlation matrix and covariance matrix should be printed.
}
  \item{effect}{Character string defining the type of effect, one of "unweighted" and "weighted".
}
}

\value{

   \item{Data.Info }{List of samples and sample sizes and estimated effect per group.}
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
F. Konietschke, L.A. Hothorn, E. Brunner: Rank-Based Multiple Test Procedures and
Simultaneous Confidence Intervals. Electronic Journal of Statistics, Vol.0 (2011) 1-8.

Konietschke, F., Placzek, M., Schaarschmidt, S., Hothorn, L.A. (2014). nparcomp: An R Software Package for Nonparametric Multiple Comparisons and Simultaneous Confidence Intervals. Journal of Statistical Software, 61(10), 1-17.       
}
\author{ Frank Konietschke}
\note{ If the samples are completely seperated the variance estimators are Zero by construction. In these cases the Null-estimators
are replaced
by 0.001.
Estimated relative effects with 0 or 1 are replaced with 0.001, 0.999 respectively.

A summary and a graph can be created separately by using the functions
\code{\link{summary.mctp}} and \code{\link{plot.mctp}}.

For the analysis, the R packages 'multcomp' and 'mvtnorm' are required.
}



\seealso{ For simultaneous confidence intervals for relative contrast effects, see \code{\link{nparcomp}}. }
\examples{

data(liver)

  # Williams Contrast

a<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
        type = "Williams", alternative = "two.sided", 
        plot.simci = TRUE, info = FALSE)
summary(a)

 # Dunnett Contrast

b<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
        type = "Dunnett", alternative = "two.sided", 
        plot.simci = TRUE, info = FALSE)
summary(b)

 # Dunnett dose 3 is baseline

c<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
        type = "Dunnett", control = "3",alternative = "two.sided",
        plot.simci = TRUE, info = FALSE)
summary(c)


data(colu)

  # Tukey comparison- one sided(less)

a<-mctp(corpora~ dose, data=colu, asy.method = "mult.t",
        type = "Tukey",alternative = "less", 
        plot.simci = TRUE, info = FALSE)
summary(a)

 # Tukey comparison- one sided(greater)

b<-mctp(corpora~ dose, data=colu, asy.method = "mult.t",
        type = "Tukey",alternative = "greater", 
        plot.simci = TRUE, info = FALSE)
summary(b)

  # Tukey comparison- one sided(less)

c<-mctp(corpora~ dose, data=colu, asy.method = "mult.t",
        type = "Tukey",alternative = "less", 
        plot.simci = TRUE, info = FALSE)
summary(c)

 # Marcus comparison- one sided(greater)

d<-mctp(corpora~ dose, data=colu, asy.method = "fisher",
        type = "Marcus",alternative = "greater", 
        plot.simci = TRUE, info = FALSE)
summary(d)
}

\keyword{htest}

\concept{ Nonparametric }
\concept{ Contrast }
\concept{ Relative Effect }
\concept{ Simultaneous Confidene Intervals }