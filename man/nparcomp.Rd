\name{nparcomp}
\alias{nparcomp}

\title{ Nonparametric relative contrast effects: Simultaneous confidence intervals and p-values }

\description{
 The function nparcomp computes the estimator of nonparametric relative contrast effects, simultaneous confidence intervals for
 the effects and simultaneous p-values based on special contrasts like "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint",
 "AVE", "McDermott", "Marcus". The statistics are computed using multivariate normal distribution, multivariate Satterthwaite t-Approximation
 and multivariate transformations (Probit and Logit transformation function).
The function 'nparcomp' also computes one-sided and two-sided confidence intervals and p-values. The confidence intervals are plotted.
}

\usage{
nparcomp(formula, data,
 type = c("UserDefined", "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint",
 "AVE", "McDermott", "Marcus", "UmbrellaWilliams"), control = NULL, conflevel = 0.95,
 alternative = c("two.sided", "lower", "greater"), rounds = 3, correlation = FALSE,
 asy.method = c("logit", "probit", "normal", "mult.t"), plot.simci = TRUE, info = TRUE,
 contrast.matrix = NULL)
}

\arguments{
 \item{formula}{  \code{formula}  A two-sided 'formula' specifying a numeric response variable
          and a factor with more than two levels. If the factor contains less than 3 levels, an error message will be returned }
  \item{data}{  \code{data} A dataframe containing the variables specified in formula}
  \item{type}{  \code{type} Character string defining the type of contrast. It should be one of "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus", "UmbrellaWilliams" }
  \item{control}{  \code{control} Character string defining the control group in Dunnett comparisons. By default it is the first group by
 lexicographical ordering  }
 \item{conflevel}{ \code{conflevel} The confidence level for the 1 - conflevel confidence intervals. By default it is 0.05 }
 \item{alternative}{ \code{alternative} Character string defining the alternative hypothesis, one
          of "two.sided", "lower" or "greater" }
  \item{rounds}{ Number of rounds for the  numeric values of the output. By default it is rounds=3 }
  \item{correlation}{  \code{correlation} A logical whether the estimated correlation matrix and covariance matrix should be printed  }
  \item{asy.method}{  \code{asy.method} character string defining the asymptotic approximation method, one
          of "logit", for using the logit transformation function, "probit", for using the probit transformation function, "normal",
          for using the multivariate normal distribution or "mult.t" for
          using a multivariate t-distribution with a Satterthwaite Approximation   }
  \item{plot.simci}{  \code{plot.simci} A logical indicating whether you want a plot of the confidence intervals }
  \item{info}{ \code{info} A logical whether you want a brief overview with informations about the output  }
  \item{contrast.matrix}{optional, a user defined contrast matrix, ignored if...}
}

\value{
   \item{weight.matrix }{The weight matrix for the choosen nonparametric relative contrast effect}
   \item{Data.Info }{List of samples and sample sizes }
   \item{relative.effects }{Comparison: relative contrast effect ,
                           relative.effect: estimated relative contrast effect,
                           confidence.interval: simultaneous confidence interval for relative contrast effect,
                           t.value: teststatistic
                           p.value: simultaneous p-values for the hypothesis by the choosen approximation method
                          }
}
\references{
 Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample Approximations,
 Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Simultaneous Confidence Intervals for Relative Effects in Dunnett Comparsisons,
 Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43, 553-569.  }

\author{ Frank Konietschke}

\note{ If the samples are completely seperated the variance estimators are Zero by construction. In these cases the Null-estimator
are replaced
by 0.001.
Estimated relative effects with 0 or 1 are replaced with 0.001, 0.999 respectively.
For the analysis, the R packages 'multcomp' and 'mvtnorm' are required.
}

\seealso{ For two-sample comparisons based on relative effects, see \code{\link{npar.t.test}} }
\examples{

data(liver)

  # Williams Contrast

nparcomp(weight ~dosage, data=liver, asy.method = "probit",
 type = "Williams", alternative = "two.sided", plot.simci = TRUE, info = TRUE)

 # Dunnett Contrast

nparcomp(weight ~dosage, data=liver, asy.method = "probit",
 type = "Dunnett", alternative = "two.sided", plot.simci = TRUE, info = TRUE) 

 # Dunnett dose 3 is baseline

nparcomp(weight ~dosage, data=liver, asy.method = "probit",
 type = "Dunnett", control = "3",alternative = "two.sided",
 plot.simci = TRUE, info = TRUE)


data(colu)


\dontrun{

 # Tukey comparison- one sided(lower)

nparcomp(corpora ~dose, data=colu, asy.method = "mult.t",
 type = "Tukey",alternative = "lower", plot.simci = TRUE, info = TRUE)

 # Tukey comparison- one sided(greater)

nparcomp(corpora ~dose, data=colu, asy.method = "mult.t",
 type = "Tukey",alternative = "greater", plot.simci = TRUE, info = TRUE) 

  # Tukey comparison- one sided(lower)

nparcomp(corpora ~dose, data=colu, asy.method = "mult.t",
 type = "Tukey",alternative = "lower", plot.simci = TRUE, info = TRUE)
}


 # Marcus comparison- one sided(greater)

nparcomp(corpora ~dose, data=colu, asy.method = "logit",
 type = "Marcus",alternative = "greater", plot.simci = TRUE, info = TRUE) 


}

\keyword{htest}

\concept{ Nonparametric }
\concept{ Contrast }
\concept{ Relative Effect }
\concept{ Simultaneous Confidene Intervals }