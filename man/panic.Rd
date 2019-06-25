\name{panic}
\alias{panic}
\docType{data}
\title{
Clinical Global Impression (CGI) Scores
}
\description{
Scores for the clinical global impression (CGI) measured on an ordinal scale 
(ranging from 2 to 8) during eight weeks for 16 patients with panic disorder 
attacks in a psychiatric clinical trial.
}
\usage{data(panic)}
\format{
  A data frame with 80 observations on the following 2 variables.
  \describe{
    \item{\code{CGI}}{A numeric vector containing the CGI score.}
    \item{\code{week}}{A numeric vector indicating the week (0,2,4,6,8) of measurement.}
  }
}
\details{
Note that the first observation in each week corresponds to the first patient, the 
second one to the second patient, and so on. There are 5 repeated measures per 
patient.
}
\source{
Brunner, E., Domhof, S., Langer, F. (2002): Nonparametric Analysis of Longitudinal Data in Factorial Experiments. Wiley, New York.
}

\examples{
\dontrun{
data(panic)
boxplot(CGI~week,data=panic)
}}
\keyword{datasets}
