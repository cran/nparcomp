\name{nparcomp-package}
\alias{nparcomp-package}
\docType{package}
\title{
Nparcomp: Nonparametric relative contrast effects. With this package it is  possible to compute nonparametric relative contrast effects and
simultaneous confidence intervals for the effects.
}
\description{
With this package, it is possible to compute nonparametric simultaneous
             confidence intervals for relative contrast effects in the
	     unbalanced one way layout. Moreover, it computes simultaneous p-values. The simultaneous confidence intervals can
	     be computed using multivariate normal distribution, multivariate t-distribution with a Satterthwaite
	     Approximation of the degree of freedom or using multivariate range preserving transformations with
	     Logit or Probit as transformation function. 2 sample comparisons can be performed with the same
             methods described above. There is no assumption on the underlying distribution function, only that
	     the data have to be at least ordinal numbers.
}
\details{
\tabular{ll}{
Package: \tab nparcomp\cr
Type: \tab Package\cr
Version: \tab 1.0-1\cr
Date: \tab 2011-02-17\cr
License: \tab GPL\cr
}

}
\author{
Frank Konietschke

Maintainer: Frank Konietschke <fkoniet@gwdg.de>
}

\references{
Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample Approximations,
 Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Simultaneous Confidence Intervals for Relative Effects in Dunnett Comparsisons,
 Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43, 553-569.
}
\keyword{ package }
\keyword{ htest }

\examples{

# two sample comparisons: Nonparametric Behrens-Fisher Problem

data(impla)
npar.t.test(impla~group, data = impla,
 asy.method = "t.app", p.perm = TRUE, alternative = "two.sided")

npar.t.test(impla~group, data = impla,
 asy.method = "logit", p.perm = TRUE, alternative = "two.sided")

   # no permutation test

npar.t.test(impla~group, data = impla,
 asy.method = "t.app", p.perm = FALSE, alternative = "two.sided")

#--Analysis of relative contrast effects in different contrast settings


data(liver)

 # Williams Contrast    

nparcomp(weight ~dosage, data=liver, asy.method = "probit",
 type = "Williams", alternative = "two.sided",
 plot.simci = TRUE, info = TRUE)

 # Dunnett Contrast

nparcomp(weight ~dosage, data=liver, asy.method = "probit",
 type = "Dunnett", alternative = "two.sided",
 plot.simci = TRUE, info = TRUE) 

 # Dunnett dose 3 is baseline

nparcomp(weight ~dosage, data=liver, asy.method = "probit",
 type = "Dunnett", control = "3",alternative = "two.sided",
 plot.simci = TRUE, info = TRUE) 



data(colu)


\dontrun{

  # Tukey comparison - one sided(lower)

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
