##' Summary of \code{\link{npar.t.test}}
##' 
##' The function summary.nparttestpaired produces a result summary of
##' \code{\link{npar.t.test.paired}}. It can only be applied to objects of
##' class "nparttestpaired".
##' 
##' Since summary.nparttestpaired is a S3 method it suffices to use summary(x)
##' as long as x is of class "nparttestpaired". It will be interpreted as
##' summary.nparttestpaired(x).
##' 
##' @param object An object of class "nparttestpaired", i.e. the result when
##' applying \code{\link{npar.t.test.paired}} to a dataset. Otherwise an error
##' will occur.
##' @param ... Arguments to be passed to methods.
##' @return The function produces a summary of the result of
##' \code{\link{npar.t.test.paired}} starting with some global information:
##' alternative hypothesis, confidence level, interpretation. This is followed
##' by: \item{ Info }{List of samples and sample sizes. } \item{Analysis
##' }{Effect: relative effect p(a,b) of the two samples 'a' and 'b', p.hat:
##' estimated relative effect, Lower: Lower limit of the confidence interval,
##' Upper: Upper limit of the confidence interval, T: teststatistic p.value:
##' p-value for the hypothesis by the choosen approximation method.  }
##' @note You can create a graphical result of the nonparametric paired t-test
##' by using the function \code{\link{plot.nparttestpaired}}.
##' @author Frank Konietschke
##' @seealso For further information on the usage of npar.t.test.paired, see
##' \code{\link{npar.t.test.paired}}.
##' @references Munzel, U., Brunner, E. (2002). An Exact Paired Rank Test.
##' Biometrical Journal 44, 584-593.
##' 
##' Konietschke, F., Pauly, M. (2012). A Studentized Permutation Test for the
##' Nonparametric Behrens-Fisher Problem in Paired Data. Electronic Journal of
##' Statistic, Vol 6, 1358-1372.
##' @keywords print
##' @examples
##' 
##' data(PGI)
##' a<-npar.t.test.paired(PGIscore~timepoint, data = PGI, 
##'                alternative = "two.sided", info=FALSE, plot.simci=FALSE)
##' summary(a)
##' 
##' 
summary.nparttestpaired <-
function(object,...)
{         
            cat("\n", "# Nonparametric Paired t Test Procedures and Confidence Intervals for the relative  effect #", "\n","\n",
        "-", "Alternative Hypothesis: ", object$text.Output,"\n",
        "-", "Confidence level:", object$input$conf.level*100,"%", "\n", "-", "Method", "=", object$Method,
                           "\n","#---------------------------Interpretation-------------------------------------------------#",
            "\n", "p(a,b)", ">", "1/2", ":", "b tends to be larger than a","\n",
                  "#------------------------------------------------------------------------------------------#","\n",

            "\n")
            cat( " #----Data Info-----------------------------------------------------------------------------#","\n")
            print(object$Info)
            cat("\n", "#----Analysis------------------------------------------------------------------------------#","\n")
            print(object$Analysis)
            cat("\n", "#------------------------------------------------------------------------------------------#","\n")

}
