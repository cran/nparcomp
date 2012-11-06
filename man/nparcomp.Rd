\name{nparcomp}
\alias{nparcomp}

\title{ Nonparametric relative contrast effects}

\description{
 The function nparcomp computes the estimator of nonparametric relative contrast effects, simultaneous confidence intervals for
 the effects and simultaneous p-values based on special contrasts like "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint",
 "AVE", "McDermott", "Marcus", "UmbrellaWilliams", "UserDefined". The statistics are computed using multivariate normal distribution, multivariate Satterthwaite t-Approximation
 and multivariate transformations (Probit and Logit transformation function).
The function 'nparcomp' also computes one-sided and two-sided confidence intervals and p-values. The confidence intervals can be plotted.
}

\usage{
nparcomp(formula, data, type = c("Tukey", "Dunnett",
         "Sequen", "Williams", "Changepoint", "AVE", "McDermott",
         "Marcus", "UmbrellaWilliams", "UserDefined"), control = NULL, 
         conf.level = 0.95, alternative = c("two.sided", "less", 
         "greater"), rounds = 3, correlation = FALSE, 
         asy.method = c("logit", "probit", "normal", "mult.t"), 
         plot.simci = FALSE, info = TRUE, contrast.matrix=NULL, 
         weight.matrix=FALSE)
}

\arguments{
 \item{formula}{A two-sided 'formula' specifying a numeric response variable
          and a factor with more than two levels. If the factor contains less than 3 levels, an error message will be returned. }
  \item{data}{A dataframe containing the variables specified in formula.}
  \item{type}{Character string defining the type of contrast. It should be one of  
  "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus", "UmbrellaWilliams", "UserDefined". }
  \item{control}{Character string defining the control group in Dunnett comparisons. By default it is the first group by
definition of the dataset.  }
 \item{conf.level}{The confidence level for the conflevel confidence intervals (default is 0.95). }
 \item{alternative}{Character string defining the alternative hypothesis, one
          of "two.sided", "less" or "greater". }
  \item{rounds}{ Number of rounds for the  numeric values of the output. By default it is rounds=3. }
  \item{correlation}{A logical whether the estimated correlation matrix and covariance matrix should be printed.  }
  \item{asy.method}{Character string defining the asymptotic approximation method, one
          of "logit", for using the logit transformation function, "probit", for using the probit transformation function, "normal",
          for using the multivariate normal distribution or "mult.t" for
          using a multivariate t-distribution with a Satterthwaite Approximation. }
  \item{plot.simci}{A logical indicating whether you want a plot of the confidence intervals. }
  \item{info}{A logical whether you want a brief overview with informations about the output.  }
  \item{contrast.matrix}{User defined contrast matrix.}
  \item{weight.matrix}{A logical indicating whether the weight matrix should be printed.}
}

\value{

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
   \item{ input }{List of input by user.}
}
\references{
 Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample Approximations.
 
 Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43, 553-569.  }

\author{ Frank Konietschke}

\note{ If the samples are completely seperated the variance estimators are Zero by construction. In these cases the Null-estimators
are replaced
by 0.001.
Estimated relative effects with 0 or 1 are replaced with 0.001, 0.999 respectively.

A summary and a graph can be created separately by using the functions
\code{\link{summary.nparcomp}} and \code{\link{plot.nparcomp}}.

For the analysis, the R packages 'multcomp' and 'mvtnorm' are required.
}

\seealso{ For two-sample comparisons based on relative effects, see \code{\link{npar.t.test}}. }
\examples{

data(liver)

  # Williams Contrast

a<-nparcomp(weight ~dosage, data=liver, asy.method = "probit",
            type = "Williams", alternative = "two.sided", 
            plot.simci = TRUE, info = FALSE,correlation=TRUE)
summary(a)



 # Dunnett dose 3 is baseline

c<-nparcomp(weight ~dosage, data=liver, asy.method = "probit",
            type = "Dunnett", control = "3",
            alternative = "two.sided", info = FALSE)
summary(c)
plot(c)


data(colu)

  # Tukey comparison- one sided(lower)

a<-nparcomp(corpora~ dose, data=colu, asy.method = "mult.t",
            type = "Tukey",alternative = "less", 
            plot.simci = TRUE, info = FALSE)
summary(a)


 # Tukey comparison- one sided(greater)

b<-nparcomp(corpora~ dose, data=colu, asy.method = "mult.t",
            type = "Tukey",alternative = "greater", 
            plot.simci = TRUE, info = FALSE)
summary(b)


}

\keyword{htest}

\concept{ Nonparametric }
\concept{ Contrast }
\concept{ Relative Effect }
\concept{ Simultaneous Confidene Intervals }