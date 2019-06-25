##' Visualizing the result of \code{\link{npar.t.test}}
##' 
##' This function takes an object of class "nparttest" and creates a plot of
##' the confidence interval for the estimated effect.
##' 
##' It is not possible to change any parameter set in the
##' \code{\link{npar.t.test}}-statement.
##' 
##' Since plot.nparttest is a S3 method it suffices to use plot(x) as long as x
##' is of class "nparttest". It will be interpreted as plot.nparttest(x).
##' 
##' @param x \code{x} An object of class "nparttest", i.e. the result when
##' applying \code{\link{npar.t.test}} to a dataset. Otherwise an error will
##' occur.
##' @param ... \code{...} Arguments to be passed to methods.
##' @return plot.npar.t.test returns a graph that contains a confidence
##' interval for the estimated effect of the nonparametric t-test. It just
##' visualizes the result of the \code{\link{npar.t.test}}-statement.
##' @note It is possible to create a graphical result of the nonparametric
##' t-test directly by setting plot.simci=TRUE in the
##' \code{\link{npar.t.test}}-statement.
##' @author Frank Konietschke
##' @seealso For further information on the usage of npar.t.test, see
##' \code{\link{npar.t.test}}.
##' @references Brunner, E., Munzel, U. (2000). The Nonparametric
##' Behrens-Fisher Problem: Asymptotic Theory and a Small Sample Approximation.
##' Biometrical Journal 42, 17 -25.
##' 
##' Neubert, K., Brunner, E., (2006). A Studentized Permutation Test for the
##' Nonparametric Behrens-Fisher Problem. Computational Statistics and Data
##' Analysis.
##' @keywords aplot
##' @examples
##' 
##' data(impla)
##' a<-npar.t.test(impla~group, data = impla, method = "t.app",
##'                alternative = "two.sided", plot.simci=FALSE)
##' plot(a)
##' 
plot.nparttest <-
function(x,...)
{
nc<-length(x$cmpid)
text.Ci<-paste(x$input$conf.level*100, "%", "Confidence Interval for p")
 Lowerp<-"|"
       plot(x$Analysis$Estimator,1:nc,xlim=c(0,1), pch=15,axes=FALSE,xlab="",ylab="")
       points(x$Analysis$Lower,1:nc, pch=Lowerp,font=2,cex=2)
              points(x$Analysis$Upper,1:nc, pch=Lowerp,font=2,cex=2)
              abline(v=0.5, lty=3,lwd=2)
              for (ss in 1:nc){
              polygon(x=c(x$Analysis$Lower[ss],x$Analysis$Upper[ss]),y=c(ss,ss),lwd=2)}
              axis(1, at = seq(0, 1, 0.1))
              axis(2,at=1:nc,labels=x$cmpid,font=2)
                box()
 title(main=c(text.Ci, paste("Method:", x$AsyMethod) ))
}
