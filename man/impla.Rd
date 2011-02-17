\name{impla}
\alias{impla}
\docType{data}
\title{ Numbers of implantations}
\description{
 Data from a fertility trial with 29 female Wistar rats: numbers of the implantations
 in a placebo group and in an active treatment group
}
\usage{data(impla)}
\format{
  A data frame with 29 observations on the following 2 variables.
  \describe{
    \item{\code{group}}{a factor with levels \code{Placebo} \code{Verum}, where Verum denotes the active treatment group}
    \item{\code{impla}}{a numeric vector}
  }
}
\details{
  The objective is to test if the active treatment influence the fertiliy of the rats
}
\source{
  Brunner, E., Munzel, U. (2002): Nichtparametrische Datenanalyse - Unverbundene Stichproben. Statistik und ihre Anwendungen, Springer, Berlin Heidelberg New York
}

\examples{
library(nparcomp)
data(impla)
boxplot(impla~group,data=impla)
}
\keyword{datasets}
