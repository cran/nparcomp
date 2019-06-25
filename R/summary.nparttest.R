##' Summary of \code{\link{npar.t.test}}
##' 
##' The function summary.npar.t.test produces a result summary of
##' \code{\link{npar.t.test}}. It can only be applied to objects of class
##' "nparttest".
##' 
##' Since summary.nparttest is a S3 method it suffices to use summary(x) as
##' long as x is of class "nparttest". It will be interpreted as
##' summary.nparttest(x).
##' 
##' @param object An object of class "nparttest", i.e. the result when applying
##' \code{\link{npar.t.test}} to a dataset. Otherwise an error will occur.
##' @param ... Arguments to be passed to methods.
##' @return The function produces a summary of the result of
##' \code{\link{npar.t.test}} starting with some global information:
##' alternative hypothesis, confidence level, interpretation. This is followed
##' by: \item{ Info }{List of samples and sample sizes. } \item{Analysis
##' }{Effect: relative effect p(a,b) of the two samples 'a' and 'b', Estimator:
##' estimated relative effect, Lower: Lower limit of the confidence interval,
##' Upper: Upper limit of the confidence interval, T: teststatistic p.Value:
##' p-value for the hypothesis by the choosen approximation method.  }
##' \item{Permutation_Test}{Result of the studentized permutation test.}
##' @note You can create a graphical result of the nonparametric t-test by
##' using the function \code{\link{plot.nparttest}}.
##' @author Frank Konietschke
##' @seealso For further information on the usage of npar.t.test, see
##' \code{\link{npar.t.test}}.
##' @references Brunner, E., Munzel, U. (2000). The Nonparametric
##' Behrens-Fisher Problem: Asymptotic Theory and a Small Sample Approximation.
##' Biometrical Journal 42, 17-25.
##' 
##' Neubert, K., Brunner, E., (2006). A Studentized Permutation Test for the
##' Nonparametric Behrens-Fisher Problem. Computational Statistics and Data
##' Analysis.
##' @keywords print
##' @examples
##' 
##' data(impla)
##' a<-npar.t.test(impla~group, data = impla, method = "t.app",
##'                alternative = "two.sided", 
##'                plot.simci=FALSE, info=FALSE)
##' summary(a)
##' 
##' 
summary.nparttest <-
function(object,...)
{         
            cat("\n", "#-----Nonparametric Test Procedures and Confidence Intervals for relative  effects-----#", "\n","\n",
        "-", "Alternative Hypothesis: ", object$text.Output,"\n",
        "-", "Confidence level:", object$input$conf.level*100,"%", "\n", "-", "Method", "=", object$AsyMethod,
                           "\n","#---------------------------Interpretation---------------------------------------------#",
            "\n", "p(a,b)", ">", "1/2", ":", "b tends to be larger than a","\n",
                  "#--------------------------------------------------------------------------------------#","\n",

            "\n")
            cat( " #----Data Info-------------------------------------------------------------------------#","\n")
            print(object$Info)
            cat("\n", "#----Analysis--------------------------------------------------------------------------#","\n")
            print(object$Analysis)

}
