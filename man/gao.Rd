\name{gao}
\alias{gao}

\title{Nonparametric multiple test procedure for many-to-one comparisons
}
\description{
This function can be used to perform the nonparametric multiple tests for many-to-one comparisons  by Gao et al. (2008). The multiple level
is strongly controlled by the Hochberg-adjustment.
}
\usage{
gao(formula, data, alpha = 0.05, control = NULL, silent = FALSE)
}

\arguments{
  \item{formula}{A two-sided 'formula' specifying a numeric response variable
          and a factor with more than two levels. If the factor contains less than 3 levels, an error message will be returned.
}
  \item{data}{A dataframe containing the variables specified in formula.
}
  \item{alpha}{
The significance level (by default = 0.05).
}
  \item{control}{Character string defining the control group in Dunnett comparisons. By default it is the first group by
 lexicographical ordering
}
  \item{silent}{A logical indicating more informations should be print on screen.
}
}

\value{

\item{Info }{Samples and sizes with estimated relative effects and variance estimators.}
\item{Analysis }{Comparison: Distributions being compared,
                 Estimator: Estimated effect,
                 df: Degree of Freedom,
                  Statistic: Teststatistic,
                  P.Raw: Raw p-Value
                  P.Hochberg: Adjusted p-Value by the Hochberg adjustment,
                  Rejected: A logical indicating rejected hypotheses,
                  P.Bonf: Bonferroni adjusted p-Values,
                  P.Holm: Holm adjusted p-Value.         }

}
\references{
Gao, X. et al. (2008). Nonparametric Multiple Comparison Procedures for Unbalanced One-Way Factorial Designs. JSPI 138, 2574 - 2591.
}
\author{
Frank Konietschke
}
\note{The procedure can only be used to test hypotheses in terms of the distribution functions.
}



\seealso{
For nonparametric all-pairs comparison see \code{\link{gao_cs}}.
}
\examples{

data(liver)

gao(weight ~dosage, data=liver,alpha=0.05)

 # Control= 3

gao(weight ~dosage, data=liver,alpha=0.05,control="3")
}

\keyword{htest}

\concept{ Nonparametric }
\concept{ Contrast }
\concept{ Relative Effect }
\concept{ Simultaneous Confidene Intervals }