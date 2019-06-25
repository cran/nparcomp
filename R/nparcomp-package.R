

##' Appetite scores of colorectal cancer patients
##' 
##' Data from one of the quality of life measurements collected from colorectal
##' cancer patients enrolled in the North Central Cancer Treatment Group phase
##' III trials N9741. The patient received three treatment regimens: IFL
##' (irinotecan, bolus fluorouracil, and leucovorin), FOLFOX (infused
##' fluorouracil, leucovorin, and ocaliplatin), and IROX (irinotecan and
##' oxaliplatin).
##' 
##' The objective is to test whether there are differences between the
##' treatment regimens in terms of different appetite scores.
##' 
##' @name appetite
##' @docType data
##' @format A data frame with 174 observations on the following 2 variables.
##' \describe{ \item{list("Group")}{A factor with levels \code{FOLFOX}
##' \code{IFL} \code{IROX}.} \item{list("Score")}{A numeric vector containing
##' the appetite scores.} }
##' @source Ryu, E. (2009): Simultaneous confidence intervals using ordinal
##' effect measures for ordered categorical outcomes. Statistics In Medicine,
##' 28(25), 3179-3188.
##' @keywords datasets
##' @examples
##' 
##' library(nparcomp)
##' data(appetite)
##' 
NULL





##' Numbers of corpora lutea
##' 
##' Data from a fertility trial with 92 female Wistar rats: numbers of the
##' corpora lutea in a placebo group and in 4 dose groups with an increasing
##' dose of an active treatment.
##' 
##' The objective is to test if the active treatment influences the fertiliy of
##' the rats.
##' 
##' @name colu
##' @docType data
##' @format A data frame with 92 observations on the following 2 variables.
##' \describe{ \item{list("dose")}{A factor with levels \code{dose1},
##' \code{dose2}, \code{dose3}, \code{dose4}, \code{Placebo}, where Placebo is
##' the placebo group and dose1-dose4 are the 4 dose groups with an increasing
##' dose.} \item{list("corpora")}{A numeric vector containing the numbers of
##' the corpora lutea.} }
##' @source Brunner, E., Munzel, U. (2002): Nichtparametrische Datenanalyse -
##' Unverbundene Stichproben. Statistik und ihre Anwendungen, Springer-Verlag.
##' @keywords datasets
##' @examples
##' 
##' library(nparcomp)
##' data(colu)
##' boxplot(corpora~dose,data=colu)
##' 
NULL





##' Numbers of implantations
##' 
##' Data from a fertility trial with 29 female Wistar rats: numbers of the
##' implantations in a placebo group and in an active treatment group.
##' 
##' The objective is to test if the active treatment influences the fertiliy of
##' the rats.
##' 
##' @name impla
##' @docType data
##' @format A data frame with 29 observations on the following 2 variables.
##' \describe{ \item{list("group")}{A factor with levels \code{Placebo},
##' \code{Verum}, where Verum denotes the active treatment group.}
##' \item{list("impla")}{A numeric vector.} }
##' @source Brunner, E., Munzel, U. (2002): Nichtparametrische Datenanalyse -
##' Unverbundene Stichproben. Statistik und ihre Anwendungen, Springer-Verlag.
##' @keywords datasets
##' @examples
##' 
##' library(nparcomp)
##' data(impla)
##' boxplot(impla~group,data=impla)
##' 
NULL





##' Relative liver weights
##' 
##' Data from a toxicity trial with male Wistar rats: Relative liver weights in
##' a negative control group and in 4 dose groups with an increasing dose of an
##' active treatment. After treatment the relative liver weights of the rats
##' were computed.
##' 
##' The objective is to test if the active treatment influences the liver
##' weight of the rats.
##' 
##' @name liver
##' @docType data
##' @format A data frame with 38 observations on the following 2 variables.
##' \describe{ \item{list("dosage")}{A numeric vector indicating the
##' dose/control group.} \item{list("weight")}{A numeric vector containing the
##' relative liver weights.} }
##' @source Brunner, E., Munzel, U. (2002): Nichtparametrische Datenanalyse -
##' Unverbundene Stichproben. Statistik und ihre Anwendungen, Springer-Verlag.
##' @keywords datasets
##' @examples
##' 
##' data(liver)
##' boxplot(weight~dosage,data=liver)
##' 
NULL





##' Nparcomp: Nonparametric relative contrast effects.
##' 
##' With this package, it is possible to compute nonparametric simultaneous
##' confidence intervals for relative contrast effects in the unbalanced one
##' way layout. Moreover, it computes simultaneous p-values. The simultaneous
##' confidence intervals can be computed using multivariate normal
##' distribution, multivariate t-distribution with a Satterthwaite
##' Approximation of the degree of freedom or using multivariate range
##' preserving transformations with Logit or Probit as transformation function.
##' 2 sample comparisons can be performed with the same methods described
##' above. There is no assumption on the underlying distribution function, only
##' that the data have to be at least ordinal numbers.
##' 
##' \tabular{ll}{ Package: \tab nparcomp\cr Type: \tab Package\cr Version: \tab
##' 1.0-0\cr Date: \tab 2012-06-22\cr License: \tab GPL\cr }
##' 
##' @name nparcomp-package
##' @docType package
##' @author Frank Konietschke
##' 
##' Maintainer: Frank Konietschke <fkoniet@@gwdg.de>
##' @references Konietschke, F. (2009). Simultane Konfidenzintervalle fuer
##' nichtparametrische relative Kontrasteffekte. PhD-thesis, University of
##' Goettingen.
##' 
##' Konietschke, F., Brunner, E., Hothorn, L.A. (2008). Nonparametric Relative
##' Contrast Effects: Asymptotic Theory and Small Sample Approximations,
##' Research report.
##' 
##' Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank
##' Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43,
##' 553-569.
##' @keywords package htest
##' @examples
##' 
##' 
##' # two sample comparisons: Nonparametric Behrens-Fisher Problem
##' 
##' data(impla)
##' a<-npar.t.test(impla~group, data = impla,
##'                method = "t.app", 
##'                alternative = "two.sided")
##' summary(a)
##' plot(a)
##' 
##' 
##' 
##' #--Analysis of relative contrast effects in different contrast settings
##' 
##' data(liver)
##' 
##'  # Williams Contrast
##' 
##' a<-nparcomp(weight ~dosage, data=liver, asy.method = "probit",
##'  type = "Williams", alternative = "two.sided",
##'  plot.simci = TRUE, info = FALSE)
##' summary(a)
##' 
##' 
##'  # Dunnett dose 3 is baseline
##' 
##' c<-nparcomp(weight ~dosage, data=liver, asy.method = "probit",
##'  type = "Dunnett", control = "3",alternative = "two.sided",
##'  plot.simci = TRUE, info = FALSE)
##' summary(c)
##' 
##' 
##' 
##' data(colu)
##' 
##'   # Tukey comparison - one sided(lower)
##' 
##' a<-nparcomp(corpora~ dose, data=colu, asy.method = "mult.t",
##'  type = "Tukey",alternative = "less")
##' summary(a)
##' plot(a)
##' 
##'   # Tukey comparison- one sided(greater)
##' 
##' b<-nparcomp(corpora~ dose, data=colu, asy.method = "mult.t",
##'  type = "Tukey",alternative = "greater")
##' summary(b)
##' plot(b)
##' 
##' 
NULL





##' Clinical Global Impression (CGI) Scores
##' 
##' Scores for the clinical global impression (CGI) measured on an ordinal
##' scale (ranging from 2 to 8) during eight weeks for 16 patients with panic
##' disorder attacks in a psychiatric clinical trial.
##' 
##' Note that the first observation in each week corresponds to the first
##' patient, the second one to the second patient, and so on. There are 5
##' repeated measures per patient.
##' 
##' @name panic
##' @docType data
##' @format A data frame with 80 observations on the following 2 variables.
##' \describe{ \item{list("CGI")}{A numeric vector containing the CGI score.}
##' \item{list("week")}{A numeric vector indicating the week (0,2,4,6,8) of
##' measurement.} }
##' @source Brunner, E., Domhof, S., Langer, F. (2002): Nonparametric Analysis
##' of Longitudinal Data in Factorial Experiments. Wiley, New York.
##' @keywords datasets
##' @examples
##' 
##' data(panic)
##' boxplot(CGI~week,data=panic)
##' 
NULL





##' Patient Rated Global Impression (PGI) Scores
##' 
##' Scores for the patient rated global impression (PGI) measured on an ordinal
##' scale (ranging from 1 to 6) being observed at baseline and after 4 weeks of
##' treatment. The lower the score, the better the clinical impression.
##' 
##' 
##' @name PGI
##' @docType data
##' @format A data frame with 30 observations on the following 3 variables.
##' \describe{ \item{list("patient")}{A numeric vector indicating the
##' patients.} \item{list("timepoint")}{A numeric vector indicating the week
##' (0,2,4,6,8) of measurement.} \item{list("PGIscore")}{A numeric vector
##' containing the PGI score.} }
##' @source Munzel, U., Brunner, E. (2002). An Exact Paired Rank Test.
##' Biometrical Journal 44, 584-593.
##' @keywords datasets
##' @examples
##' 
##' data(PGI)
##' boxplot(PGIscore~timepoint,data=PGI)
##' 
NULL





##' Reaction times of mice [sec]
##' 
##' Data from a toxicity trial with 40 mice.
##' 
##' The objective is to test if the active treatment influences the reaction
##' time of the mice.
##' 
##' @name reaction
##' @docType data
##' @format A data frame with 40 observations on the following 2 variables.
##' \describe{ \item{list("Group")}{A numeric vector indicating the group.}
##' \item{list("Time")}{A numeric vector containing the reaction times.} }
##' @references Shirley, E. (1977). Nonparametric Equivalent of Williams Test
##' for Contrasting Increasing Dose Levels of a Treatment. Biometrics 33, 386 -
##' 389.
##' @source Shirley, E. (1977). Nonparametric Equivalent of Williams Test for
##' Contrasting Increasing Dose Levels of a Treatment. Biometrics 33, 386 -
##' 389.
##' @keywords datasets
##' @examples
##' 
##' library(nparcomp)
##' data(reaction)
##' boxplot(Time~Group,data=reaction)
##' 
NULL



