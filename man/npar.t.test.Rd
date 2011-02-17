\name{npar.t.test}
\alias{npar.t.test}

\title{The nonparametric Behrens-Fisher problem - confidence interval for a relative effect of two groups }

\description{
 The function npar.t.test performs two sample tests for the nonparametric Behrens-Fisher problem, that is testing the hypothesis \deqn{H_0: p=1/2}{H0: p=1/2}, where
 p denotes the relative effect of 2 independent samples and computes confidence intervals for the relative effect p. The statistics are computed using
 standard normal distribution, Satterthwaite t-Approximation and variance stabilising transformations (Probit and Logit transformation function).
 For small samples there is also a studentized permutation test implemented.
 npar.t.test also computes one-sided and two-sided confidence intervals and p-values. The confidence interval is plotted.
}

\usage{
npar.t.test(formula, data, conflevel = 0.05,
 alternative = c("two.sided", "lower", "greater"), rounds = 3,
 asy.method = c("logit", "probit", "normal", "t.app"),
 p.permu = TRUE, plot.simci = TRUE, info = TRUE)
}

\arguments{
  \item{formula}{  \code{formula}  A two-sided 'formula' specifying a numeric response variable
          and a factor with two levels. If the factor contains more than two levels, an error message will be returned }
  \item{data}{  \code{data} A dataframe containing the variables specified in formula}
   \item{conflevel}{ \code{conflevel} The confidence level for the 1 - conflevel confidence intervals. By default it is 0.05 }
   \item{alternative}{ \code{alternative} character string defining the alternative hypothesis, one
          of "two.sided", "lower" or "greater" }
  \item{rounds}{ Number of rounds for the numeric values of the output. By default it is rounds=3 }
  \item{asy.method}{ \code{asy.method}  character string defining the asymptotic approximation method, one
          of "logit", for using the logit transformation function, "probit", for using the probit transformation function, "normal",
          for using the standard normal distribution or "t.app" for
          using a t-Distribution with a Satterthwaite Approximation   }
  \item{p.permu}{  \code{p.permu} A logical indicating whether you want to get the p-value of a studentized permutation test}
  \item{plot.simci}{ \code{plot.simci} A logical indicating whether you want a plot of the confidence interval }
  \item{info}{  \code{info} A logical whether you want a brief overview with informations about the output }
}

\value{

  \item{Data.Info }{List of samples and sample sizes }
  \item{relative.effects }{Comparison: relative effect p(a,b) of the two samples 'a' and 'b',
                           relative.effect: estimated relative effect,
                           confidence.interval: confidence interval for relative effect,
                           t.value: teststatistic
                           p.value: p-value for the hypothesis by the choosen approximation method,
                           p.perm : p-value of the permutation test}
  \item{Wilcoxon.Test}{Comparison: relative effect p(a,b) of the samples 'a' and 'b',
                       rel.effect: estimated relative effect,
                       p.value: p-Value of an asymptotic Wilcoxon test  }

}
\references{
Brunner, E., Munzel, U. (2000). The Nonparametric Behrens-Fisher Problem: Asymptotic Theory and a Small Sample Approximation. Biometrical Journal 42, 17 -25.
Neubert, K., Brunner, E., (2006). A Studentized Permutation Test for the Nonparametric Behrens-Fisher Problem. Computational Statistics and Data Analysis.

}

\author{ Frank Konietschke }

\note{ If the samples are completely seperated the variance estimator are Zero by construction. In these cases the Null-estimator are replaced by a replacing
method as proposed in the paper from Neubert and Brunner (2006).
Estimated relative effects with 0 or 1 are replaced with 0.001, 0.999 respectively.
}

\seealso{ For multiple comparison procedures based on relative effects, see \code{\link{nparcomp}} }
\examples{

data(impla)
npar.t.test(impla~group, data = impla, asy.method = "t.app",
 p.perm = TRUE, alternative = "two.sided")

}
\keyword{ htest }

\concept{ Nonparametric }
\concept{ Relative Effect }
\concept{ Nonparametric Behrens-Fisher Problem}
