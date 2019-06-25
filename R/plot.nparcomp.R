##' Visualizing the result of \code{\link{nparcomp}}
##' 
##' This function takes an object of class "nparcomp" and creates a plot of the
##' confidence intervals for the estimated nonparametric contrast effects.
##' 
##' It is not possible to change any parameter set in the
##' \code{\link{nparcomp}}-statement.
##' 
##' Since plot.nparcomp is a S3 method it suffices to use plot(x) as long as x
##' is of class "nparcomp". It will be interpreted as plot.nparcomp(x).
##' 
##' @param x An object of class "nparcomp", i.e. the result when applying
##' \code{\link{nparcomp}} to a dataset. Otherwise an error will occur.
##' @param ... Arguments to be passed to methods.
##' @return plot.nparcomp returns a graph that contains a confidence interval
##' for the estimated nonparametric contrast effect of each contrast. It just
##' visualizes the result of the \code{\link{nparcomp}}-statement.
##' @note It is possible to create a graphical result directly by setting
##' plot.simci=TRUE in the \code{\link{nparcomp}}-statement.
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
##' @keywords aplot
##' @examples
##' 
##' data(liver)
##' a<-nparcomp(weight ~dosage, data=liver, asy.method = "probit", 
##'             type = "Williams", alternative = "two.sided", 
##'             plot.simci = FALSE, info = FALSE)
##' plot(a)
##' 
plot.nparcomp <-
function(x,...)
{
nc<-length(x$connames)
text.Ci<-paste(x$input$conf.level*100, "%", "Simultaneous Confidence Intervals")
 Lowerp<-"|"
       plot(x$Analysis$Estimator,1:nc,xlim=c(0,1), pch=15,axes=FALSE,xlab="",ylab="")
       points(x$Analysis$Lower,1:nc, pch=Lowerp,font=2,cex=2)
              points(x$Analysis$Upper,1:nc, pch=Lowerp,font=2,cex=2)
              abline(v=0.5, lty=3,lwd=2)
              for (ss in 1:nc){
              polygon(x=c(x$Analysis$Lower[ss],x$Analysis$Upper[ss]),y=c(ss,ss),lwd=2)}
              axis(1, at = seq(0, 1, 0.1))
              axis(2,at=1:nc,labels=x$connames)
                box()
 title(main=c(text.Ci, paste("Type of Contrast:",x$input$type), paste("Method:", x$AsyMethod )))
}
