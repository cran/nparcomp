\name{gao_cs}
\alias{gao_cs}

\title{
  Nonparametric multiple test procedure for all-pairs comparisons
}
\description{
This function can be used to perform the nonparametric multiple tests for all-pairs comparisons by Gao et al. (2008). This
procedure is a nonparametric equivalent of Campbell and Skillings (1981) sequential test procedure.
}
\usage{
gao_cs(formula, data, alpha = 0.05, silent = FALSE)
}

\arguments{
  \item{formula}{A two-sided 'formula' specifying a numeric response variable
          and a factor with more than two levels. If the factor contains less than 3 levels, an error message will be returned.
}
  \item{data}{A dataframe containing the variables specified in formula.
}
  \item{alpha}{The significance level (by default = 0.05).
}
  \item{silent}{A logical indicating more informations should be print on screen.}
}

\value{

\item{Info }{Samples and sizes with estimated relative effects and variance estimators.}
\item{Single.Analysis}{Comp: Distributions being compared,
                 Effect: Estimated effect,
                 Statistic: Teststatistic,
                 DF: Degree of Freedom,
                  P.Raw: Raw p-Value,
                  P.Bonf: Bonferroni adjusted p-Values,
                  P.Holm: Holm adjusted p-Value.
                           }
\item{CS.Analysis}{Comp: Distributions being compared,
                 Effect: Estimated effect,
                 Statistic: Teststatistic,
                  DF: Degree of Freedom,
                  Quantiles: quantile,
                  Adj. P: adjusted p-Value,
                  Alpha: Significance level alpha,
                  Rejected: A logical indicating rejected hypotheses,
                  Layer: Layer of the stepwise analysis.
                           }

}
\references{
Gao, X. et al. (2008). Nonparametric Multiple Comparison Procedures for Unbalanced One-Way Factorial Designs. JSPI 138, 2574 - 2591.

Konietschke, F., Placzek, M., Schaarschmidt, S., Hothorn, L.A. (2014). nparcomp: An R Software Package for Nonparametric Multiple Comparisons and Simultaneous Confidence Intervals. Journal of Statistical Software, 61(10), 1-17.       
}
\author{
Frank Konietschke
}
\note{
The generalized Campbell and Skillings' analysis is performed in the CS.Analysis output.
The adjusted quantiles and p-Values are reported. Due to the non-monotonicity of the adjusted quantiles,
all results are checked for non-logical relations.
}



\seealso{
For nonparametric many-to-one comparison see \code{\link{gao}}.
}
\examples{

data(reaction)

gao_cs(Time ~Group, data=reaction,alpha=0.05)
}

\keyword{htest}

\concept{ Nonparametric }
\concept{ Contrast }
\concept{ Relative Effect }
\concept{ Simultaneous Confidene Intervals }