\name{npar.t.test.paired}
\alias{npar.t.test.paired}

\title{A 2-sample nonparametric studentized permutation test for paired data}

\description{
 The function npar.t.test.paired performs a two sample studentized permutation 
 test for paired data, that is testing the hypothesis \deqn{H_0: p=1/2}{H0: p=1/2,} where
 p denotes the relative effect of 2 dependent samples, and computes a confidence interval 
 for the relative effect p. In addition the Brunner-Munzel-Test accompanied by a confidence interval
 for the relative effect is implemented.
 npar.t.test.paired also computes one-sided and two-sided confidence intervals and p-values. 
 The confidence interval can be plotted.
}

\usage{
npar.t.test.paired(formula, data, conf.level = 0.95, alternative = c("two.sided",
            "less", "greater"), nperm=10000, rounds = 3, 
            info = TRUE, plot.simci = TRUE)
}

\arguments{
  \item{formula}{A two-sided 'formula' specifying a numeric response variable
          and a factor with two levels. If the factor contains more than two levels, an error message will be returned. }
  \item{data}{A dataframe containing the variables specified in formula.}
   \item{conf.level}{The confidence level (default is 0.95). }
   \item{alternative}{Character string defining the alternative hypothesis, one
          of "two.sided", "less" or "greater". }
    \item{nperm}{The number of permutations for the studentized permutation test. By default it is nperm=10,000.}  
    \item{rounds}{Number of rounds for the numeric values of the output (default is 3).}
    \item{info}{A logical whether you want a brief overview with informations about the output.} 
    \item{plot.simci}{A logical indicating whether you want a plot of the confidence interval.}
}


\value{

  \item{ Info }{List of samples and sample sizes.}
  \item{Analysis }{Effect: relative effect p(a,b) of the two samples 'a' and 'b',
                           p.hat: estimated relative effect,
                           Lower: Lower limit of the confidence interval,
                           Upper: Upper limit of the confidence interval,
                           T: studentized teststatistic
                           p.value: p-value for the hypothesis.
                           }
  \item{ input }{List of input by user.}


}
\references{
Munzel, U., Brunner, E. (2002). An Exact Paired Rank Test. Biometrical Journal 44, 584-593.

Konietschke, F., Pauly, M. (2012). A Studentized Permutation Test for the Nonparametric Behrens-Fisher Problem in Paired Data. Electronic Journal of Statistic, Vol 6, 1358-1372.

}

\author{ Frank Konietschke }

\note{
A summary and a graph can be created separately by using the functions
\code{\link{summary.nparttestpaired}} and \code{\link{plot.nparttestpaired}}.

Make sure that your dataset is ordered by subjects before applying npar.t.test.paired.
}

\seealso{ For multiple comparison procedures based on relative effects, see \code{\link{nparcomp}}. }
\examples{
\dontrun{

data(PGI)
a<-npar.t.test.paired(PGIscore~timepoint, data = PGI, 
               alternative = "two.sided", info=FALSE, plot.simci=FALSE)
summary(a)
plot(a)
               
}}
\keyword{ htest }

\concept{ Nonparametric }
\concept{ Relative Effect }
\concept{ Nonparametric Behrens-Fisher Problem in Paired Data}
