\name{npar.t.test}
\alias{npar.t.test}

\title{The nonparametric Behrens-Fisher problem}

\description{
 The function npar.t.test performs two sample tests for the nonparametric Behrens-Fisher problem, that is testing the hypothesis \deqn{H_0: p=1/2}{H0: p=1/2,} where
 p denotes the relative effect of 2 independent samples and computes confidence intervals for the relative effect p. The statistics are computed using
 standard normal distribution, Satterthwaite t-Approximation and variance stabilising transformations (Probit and Logit transformation function).
 For small samples there is also a studentized permutation test implemented.
 npar.t.test also computes one-sided and two-sided confidence intervals and p-values. The confidence interval can be plotted.
}

\usage{
npar.t.test(formula, data, conf.level = 0.95, alternative = c("two.sided",
            "less", "greater"), rounds = 3, method = c("logit",
            "probit", "normal", "t.app", "permu"), 
            plot.simci = FALSE, info = TRUE, nperm=10000)
}

\arguments{
  \item{formula}{A two-sided 'formula' specifying a numeric response variable
          and a factor with two levels. If the factor contains more than two levels, an error message will be returned. }
  \item{data}{A dataframe containing the variables specified in formula.}
   \item{conf.level}{The confidence level (default is 0.95). }
   \item{alternative}{Character string defining the alternative hypothesis, one
          of "two.sided", "less" or "greater". }
  \item{rounds}{Number of rounds for the numeric values of the output (default is 3).}
  \item{method}{Character string defining the (asymptotic approximation) method, one
          of "logit", for using the logit transformation function, "probit", for using 
          the probit transformation function, "normal",
          for using the standard normal distribution or "t.app" for
          using a t-Distribution with a Satterthwaite Approximation. 
          The studentized permutation test can be obtained by choosing "permu".}
  \item{plot.simci}{A logical indicating whether you want a plot of the confidence interval.}
  \item{info}{A logical whether you want a brief overview with informations about the output.}
    \item{nperm}{The number of permutations for the studentized permutation test. By default it is nperm=10,000.}
}


\value{

  \item{ Info }{List of samples and sample sizes.}
  \item{Analysis }{Effect: relative effect p(a,b) of the two samples 'a' and 'b',
                           Estimator: estimated relative effect,
                           Lower: Lower limit of the confidence interval,
                           Upper: Upper limit of the confidence interval,
                           T: teststatistic
                           p.Value: p-value for the hypothesis by the choosen approximation method.
                           }
  \item{ input }{List of input by user.}


}
\references{
Brunner, E., Munzel, U. (2000). The Nonparametric Behrens-Fisher Problem: Asymptotic Theory and a Small Sample Approximation. Biometrical Journal 42, 17 -25.

Neubert, K., Brunner, E., (2006). A Studentized Permutation Test for the Nonparametric Behrens-Fisher Problem. Computational Statistics and Data Analysis.

Konietschke, F., Placzek, M., Schaarschmidt, S., Hothorn, L.A. (2014). nparcomp: An R Software Package for Nonparametric Multiple Comparisons and Simultaneous Confidence Intervals. Journal of Statistical Software, 61(10), 1-17.       
}

\author{ Frank Konietschke }

\note{ If the samples are completely seperated the variance estimators are Zero by construction. In these cases the Null-estimators are replaced by a replacing
method as proposed in the paper from Neubert and Brunner (2006).
Estimated relative effects with 0 or 1 are replaced with 0.001, 0.999 respectively.

A summary and a graph can be created separately by using the functions
\code{\link{summary.nparttest}} and \code{\link{plot.nparttest}}.

}

\seealso{ For multiple comparison procedures based on relative effects, see \code{\link{nparcomp}}. }
\examples{
\dontrun{

data(impla)
a<-npar.t.test(impla~group, data = impla, method = "t.app",
               alternative = "two.sided", info=FALSE)
summary(a)
plot(a)               
b<-npar.t.test(impla~group, data = impla, method= "permu",
               alternative = "two.sided", info=FALSE)
summary(b)
plot(b)
}}
\keyword{ htest }

\concept{ Nonparametric }
\concept{ Relative Effect }
\concept{ Nonparametric Behrens-Fisher Problem}
