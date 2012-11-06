\name{liver}
\alias{liver}
\docType{data}
\title{ Relative liver weights}
\description{
Data from a toxicity trial with male Wistar rats: Relative liver weights in a negative control group and in 4 dose groups with an increasing dose of an active treatment.
After treatment the relative liver weights of the rats were computed.
}
\usage{data(liver)}
\format{
  A data frame with 38 observations on the following 2 variables.
  \describe{
    \item{\code{dosage}}{A numeric vector indicating the dose/control group.}
    \item{\code{weight}}{A numeric vector containing the relative liver weights.}
  }
}
\details{
  The objective is to test if the active treatment influences the liver weight of the rats.
}
\source{
  Brunner, E., Munzel, U. (2002): Nichtparametrische Datenanalyse - Unverbundene Stichproben. Statistik und ihre Anwendungen, Springer-Verlag.
}

\examples{
data(liver)
boxplot(weight~dosage,data=liver)
}
\keyword{datasets}
