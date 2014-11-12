\name{PGI}
\alias{PGI}
\docType{data}
\title{
Patient Rated Global Impression (PGI) Scores
}
\description{  
Scores for the patient rated global impression (PGI) measured on an ordinal scale 
(ranging from 1 to 6) being observed at baseline and after 4 weeks of treatment.
The lower the score, the better the clinical impression.
}
\usage{data(PGI)}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{patient}}{A numeric vector indicating the patients.}
    \item{\code{timepoint}}{A numeric vector indicating the week (0,2,4,6,8) of measurement.}
    \item{\code{PGIscore}}{A numeric vector containing the PGI score.}
  }
}

\source{
Munzel, U., Brunner, E. (2002). An Exact Paired Rank Test. Biometrical Journal 44, 584-593.
}

\examples{
data(PGI)
boxplot(PGIscore~timepoint,data=PGI)
}
\keyword{datasets}
