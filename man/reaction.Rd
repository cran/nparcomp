\name{reaction}
\alias{reaction}
\docType{data}
\title{
Reaction times of mice [sec]
}
\description{
Data from a toxicity trial with 40 mice.
}
\usage{data(reaction)}
\format{
  A data frame with 40 observations on the following 2 variables.
  \describe{
    \item{\code{Group}}{A numeric vector indicating the group.}
    \item{\code{Time}}{A numeric vector containing the reaction times.}
  }
}
\details{
  The objective is to test if the active treatment influences the reaction time of the mice.
}
\source{
  Shirley, E. (1977). Nonparametric Equivalent of Williams Test for Contrasting Increasing
Dose Levels of a Treatment. Biometrics 33, 386 - 389.
}
\references{
   Shirley, E. (1977). Nonparametric Equivalent of Williams Test for Contrasting Increasing
Dose Levels of a Treatment. Biometrics 33, 386 - 389.
}
\examples{
\dontrun{
library(nparcomp)
data(reaction)
boxplot(Time~Group,data=reaction)
}}
\keyword{datasets}
