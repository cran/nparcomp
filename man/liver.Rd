\name{liver}
\alias{liver}
\docType{data}
\title{ Relative liver weights}
\description{
Data from a toxicity trial with male Wistar rats: Relative liver weight in a negative control group and in 4 dose groups with an increasing dose of an active treatment.
After treatment the relative liver weight of the rats was computed.
}
\usage{data(liver)}
\format{
  A data frame with 38 observations on the following 2 variables.
  \describe{
    \item{\code{dosage}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
  }
}
\details{
  The objective is to test if the active treatment influence the liver weight of the rats.
}
\source{
  Brunner, E., Munzel, U. (2002): Nichtparametrische Datenanalyse - Unverbundene Stichproben. Statistik und ihre Anwendungen, Springer, Berlin Heidelberg New York
}
\references{

}
\examples{
data(liver)
boxplot(weight~dosage,data=liver)
}
\keyword{datasets}
