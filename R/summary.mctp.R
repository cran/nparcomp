##' Summary of \code{\link{mctp}}
##' 
##' The function summary.mctp produces a result summary of \code{\link{mctp}}.
##' It can only be applied to objects of class "mctp".
##' 
##' Since summary.mctp is a S3 method it suffices to use summary(x) as long as
##' x is of class "mctp". It will be interpreted as summary.mctp(x).
##' 
##' @param object An object of class "mctp", i.e. the result when applying
##' \code{\link{mctp}} to a dataset. Otherwise an error will occur.
##' @param ... Arguments to be passed to methods.
##' @return The function produces a summary of the result of \code{\link{mctp}}
##' starting with some global information: alternative hypothesis, estimation
##' method, type of contrast, confidence level. This is followed by:
##' \item{Data.Info }{List of samples and sample sizes and estimated effect per
##' group.} \item{Contrast}{Contrast matrix.} \item{Analysis }{Estimator:
##' Estimated relative effect, Lower: Lower limit of the simultaneous
##' confidence interval, Upper: Upper limit of the simultaneous confidence
##' interval, Statistic: Teststatistic p.Value: Adjusted p-values for the
##' hypothesis by the choosen approximation method.  }
##' @note It is possible to create a graphical result of the multiple
##' comparison test procedure by using the function \code{\link{plot.mctp}}.
##' @author Frank Konietschke
##' @seealso For further information on the usage of mctp, see
##' \code{\link{mctp}}.
##' @references F. Konietschke, L.A. Hothorn, E. Brunner: Rank-Based Multiple
##' Test Procedures and Simultaneous Confidence Intervals. Electronic Journal
##' of Statistics, Vol.0 (2011) 1-8.
##' @keywords print
##' @examples
##' 
##' data(liver)
##' a<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
##'         type = "Dunnett", alternative = "two.sided", plot.simci = FALSE, 
##'         info = FALSE)
##' summary(a)
##' 
summary.mctp <-
function(object,...)
{           
            cat("\n", "#----------------Nonparametric Multiple Comparisons for relative effects---------------#", "\n","\n",
        "-", "Alternative Hypothesis: ", object$text.Output,"\n",
        "-", "Estimation Method: Global Pseudo ranks","\n",
        "-", "Type of Contrast", ":", object$input$type, "\n", "-", "Confidence Level:",
            object$input$conf.level*100,"%", "\n", "-", "Method", "=", object$AsyMethod, "\n","\n",
                  "#--------------------------------------------------------------------------------------#","\n",

            "\n")
            cat( " #----Data Info-------------------------------------------------------------------------#","\n")
            print(object$Data.Info)
            cat("\n", "#----Contrast--------------------------------------------------------------------------#","\n")
            print(object$Contrast)
            cat("\n", "#----Analysis--------------------------------------------------------------------------#","\n")
            print(object$Analysis)
            cat("\n", "#----Overall---------------------------------------------------------------------------#","\n")
            print(object$Overall)
            if (object$input$correlation == TRUE){
            cat("\n", "#----Covariance------------------------------------------------------------------------#","\n")
            print(object$Covariance)
            cat("\n", "#----Correlation-----------------------------------------------------------------------#","\n")
            print(object$Correlation)
            }
            cat("\n", "#--------------------------------------------------------------------------------------#","\n")
}
