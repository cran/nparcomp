\name{colu}
\alias{colu}
\docType{data}
\title{ Numbers of corpora lutea}
\description{
 Data from a fertility trial with 92 female Wistar rats: numbers of the corpora lutea
 in a placebo group and in 4 dose groups with an increasing dose of an active treatment.
}
\usage{data(colu)}
\format{
  A data frame with 92 observations on the following 2 variables.
  \describe{
    \item{\code{dose}}{A factor with levels \code{dose1}, \code{dose2}, \code{dose3}, \code{dose4}, \code{Placebo}, where Placebo is the placebo
    group and dose1-dose4
    are the 4 dose groups with an increasing dose.}
    \item{\code{corpora}}{A numeric vector containing the numbers of the corpora lutea.}
  }
}
\details{
  The objective is to test if the active treatment influences the fertiliy of the rats.
}
\source{
  Brunner, E., Munzel, U. (2002): Nichtparametrische Datenanalyse - Unverbundene Stichproben. Statistik und ihre Anwendungen, Springer-Verlag.
}

\examples{
library(nparcomp)
data(colu)
boxplot(corpora~dose,data=colu)
}
\keyword{datasets}
