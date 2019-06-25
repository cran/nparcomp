##' Summary of \code{\link{nparcomp}}
##' 
##' The function summary.nparcomp produces a result summary of
##' \code{\link{nparcomp}}. It can only be applied to objects of class
##' "nparcomp".
##' 
##' Since summary.nparcomp is a S3 method it suffices to use summary(x) as long
##' as x is of class "nparcomp". It will be interpreted as summary.nparcomp(x).
##' 
##' @param object An object of class "nparcomp", i.e. the result when applying
##' \code{\link{nparcomp}} to a dataset. Otherwise an error will occur.
##' @param ... Arguments to be passed to methods.
##' @return The function produces a summary of the result of
##' \code{\link{nparcomp}} starting with some global information: alternative
##' hypothesis, estimation method, type of contrast, confidence level, method,
##' interpretation. This is followed by: \item{Data.Info }{List of samples and
##' sample sizes. } \item{Contrast}{Contrast matrix.} \item{Analysis
##' }{Comparison: relative contrast effect , relative.effect: estimated
##' relative contrast effect, Estimator: Estimated relative contrast effect,
##' Lower: Lower limit of the simultaneous confidence interval, Upper: Upper
##' limit of the simultaneous confidence interval, Statistic: Teststatistic
##' p.Value: Adjusted p-values for the hypothesis by the choosen approximation
##' method.  } \item{Overall}{Overall p-value and critical value.}
##' @note It is possible to create a graphical result of the nonparametric test
##' procedure nparcomp by using the function \code{\link{plot.nparcomp}}.
##' @author Frank Konietschke
##' @seealso For further information on the usage of nparcomp, see
##' \code{\link{nparcomp}}.
##' @references Konietschke, F., Brunner, E., Hothorn, L.A. (2008).
##' Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample
##' Approximations.
##' 
##' Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank
##' Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43,
##' 553-569.
##' @keywords print
##' @examples
##' 
##' data(liver)
##' a<-nparcomp(weight ~dosage, data=liver, asy.method = "probit", 
##'             type = "Williams", alternative = "two.sided", 
##'             plot.simci = FALSE, info = FALSE)
##' summary(a)
##' 
summary.nparcomp <-
function(object,...)
{   
            cat("\n", "#------------Nonparametric Multiple Comparisons for relative contrast effects----------#", "\n","\n",
        "-", "Alternative Hypothesis: ", object$text.Output,"\n",
        "-", "Estimation Method: Global Pseudo ranks","\n",
        "-", "Type of Contrast", ":", object$input$type, "\n", "-", "Confidence Level:",
            object$input$conf.level*100,"%", "\n", "-", "Method", "=", object$AsyMethod, "\n","\n",
        "-", "Estimation Method: Pairwise rankings","\n", "\n",
                    "#---------------------------Interpretation--------------------------------------------#",
            "\n", "p(a,b)", ">", "1/2", ":", "b tends to be larger than a","\n",
            "#-------------------------------------------------------------------------------------#", "\n",

            "\n")
            cat( " #----Data Info-------------------------------------------------------------------------#","\n")
            print(object$Data.Info)
            cat("\n", "#----Contrast--------------------------------------------------------------------------#","\n")
            print(object$Contrast)
            cat("\n", "#----Analysis--------------------------------------------------------------------------#","\n")
            print(object$Analysis)
            cat("\n", "#----Overall---------------------------------------------------------------------------#","\n")
            print(object$Overall)
            if (object$input$weight.matrix == TRUE){
            cat("\n", "#----Weight Matrix---------------------------------------------------------------------#","\n")
            print(object$Weight.Matrix)
            cat("\n", "#----All Pairs-------------------------------------------------------------------------#","\n")
            print(object$AllPairs)
            }
            if (object$input$correlation == TRUE){
            cat("\n", "#----Covariance------------------------------------------------------------------------#","\n")
            print(object$Covariance)
            cat("\n", "#----Correlation-----------------------------------------------------------------------#","\n")
            print(object$Correlation)
            }
            cat("\n", "#--------------------------------------------------------------------------------------#","\n")
}
