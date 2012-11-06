\name{appetite}
\alias{appetite}
\docType{data}
\title{Appetite scores of colorectal cancer patients
}
\description{ Data from one of the quality of life measurements collected from 
colorectal cancer patients enrolled in the North Central Cancer Treatment Group
phase III trials N9741. The patient received three treatment regimens:
IFL (irinotecan, bolus fluorouracil, and leucovorin), FOLFOX (infused 
fluorouracil, leucovorin, and ocaliplatin), and IROX (irinotecan and oxaliplatin). 
}
\usage{data(appetite)}
\format{
  A data frame with 174 observations on the following 2 variables.
  \describe{
    \item{\code{Group}}{A factor with levels \code{FOLFOX} \code{IFL} \code{IROX}.}
    \item{\code{Score}}{A numeric vector containing the appetite scores.}
  }
}
\details{
 The objective is to test whether there are differences between the treatment regimens
 in terms of different appetite scores.
}
\source{
Ryu, E. (2009): Simultaneous confidence intervals using ordinal effect measures for ordered categorical outcomes. Statistics In Medicine, 28(25), 3179-3188.
}
\examples{
library(nparcomp)
data(appetite)
}
\keyword{datasets}
