\name{nparcomp-package}
\alias{nparcomp-package}
\docType{package}
\title{
Nparcomp: Nonparametric relative contrast effects.
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
Version: \tab 1.0-0\cr
Date: \tab 2012-06-22\cr
License: \tab GPL\cr
}

}
\author{
Frank Konietschke

Maintainer: Frank Konietschke <fkoniet@gwdg.de>
}

\references{
Konietschke, F. (2009). Simultane Konfidenzintervalle fuer nichtparametrische relative Kontrasteffekte. PhD-thesis, University of Goettingen.

Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample Approximations, Research report.

Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43, 553-569.

}
\keyword{ package }
\keyword{ htest }

\examples{
\dontrun{

# two sample comparisons: Nonparametric Behrens-Fisher Problem

data(impla)
a<-npar.t.test(impla~group, data = impla,
               method = "t.app", 
               alternative = "two.sided")
summary(a)
plot(a)



#--Analysis of relative contrast effects in different contrast settings

data(liver)

 # Williams Contrast

a<-nparcomp(weight ~dosage, data=liver, asy.method = "probit",
 type = "Williams", alternative = "two.sided",
 plot.simci = TRUE, info = FALSE)
summary(a)


 # Dunnett dose 3 is baseline

c<-nparcomp(weight ~dosage, data=liver, asy.method = "probit",
 type = "Dunnett", control = "3",alternative = "two.sided",
 plot.simci = TRUE, info = FALSE)
summary(c)



data(colu)

  # Tukey comparison - one sided(lower)

a<-nparcomp(corpora~ dose, data=colu, asy.method = "mult.t",
 type = "Tukey",alternative = "less")
summary(a)
plot(a)

  # Tukey comparison- one sided(greater)

b<-nparcomp(corpora~ dose, data=colu, asy.method = "mult.t",
 type = "Tukey",alternative = "greater")
summary(b)
plot(b)

}}