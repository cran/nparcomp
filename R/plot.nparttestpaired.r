##' Visualizing the result of \code{\link{npar.t.test.paired}}
##' 
##' This function takes an object of class "nparttestpaired" and creates a plot
##' of the confidence intervals for the estimated effect resulting from the
##' studentized permutation test and the Brunner-Munzel test.
##' 
##' It is not possible to change any parameter set in the
##' \code{\link{npar.t.test.paired}}-statement.
##' 
##' Since plot.nparttestpaired is a S3 method it suffices to use plot(x) as
##' long as x is of class "nparttestpaired". It will be interpreted as
##' plot.nparttestpaired(x).
##' 
##' @param x \code{x} An object of class "nparttestpaired", i.e. the result
##' when applying \code{\link{npar.t.test.paired}} to a dataset. Otherwise an
##' error will occur.
##' @param ... \code{...} Arguments to be passed to methods.
##' @return plot.npar.t.test returns a graph that contains a confidence
##' interval for the estimated effect of the nonparametric studentized
##' permutation test as well as. It just visualizes the result of the
##' \code{\link{npar.t.test.paired}}-statement.
##' @note It is possible to create a graphical result of the nonparametric
##' studentized permutation test directly by setting plot.simci=TRUE in the
##' \code{\link{npar.t.test.paired}}-statement.
##' @author Frank Konietschke
##' @seealso For further information on the usage of npar.t.test.paired, see
##' \code{\link{npar.t.test.paired}}.
##' @references Munzel, U., Brunner, E. (2002). An Exact Paired Rank Test.
##' Biometrical Journal 44, 584-593.
##' 
##' Konietschke, F., Pauly, M. (2012). A Studentized Permutation Test for the
##' Nonparametric Behrens-Fisher Problem in Paired Data. Electronic Journal of
##' Statistic, Vol 6, 1358-1372.
##' @keywords aplot
##' @examples
##' 
##' data(PGI)
##' a<-npar.t.test.paired(PGIscore~timepoint, data = PGI, 
##'                alternative = "two.sided", info=TRUE, plot.simci=FALSE)
##' plot(a)
##' 
plot.nparttestpaired <-
function(x,...)
{
par(mfrow=c(2,1),oma=c(0,0,0,0))
text.Ci<-paste((x$input$conf.level)*100, "%", "Confidence Interval for p")  
 Lowerp<-"|"
 for (i in 1:2){
       plot(x$Analysis[i,2],1,xlim=c(0,1), pch=15,axes=FALSE,xlab="",ylab="")
       points(x$Analysis[i,1],1, pch=Lowerp,font=2,cex=2)
              points(x$Analysis[i,3],1, pch=Lowerp,font=2,cex=2)
              abline(v=0.5, lty=3,lwd=2)
              polygon(x=c(x$Analysis[i,1],x$Analysis[i,3]),y=c(1,1),lwd=2)
              axis(1, at = seq(0, 1, 0.1))
              axis(2,at=1,labels=x$methodvec[i],font=2)   
              if(i==1){title(main=c(text.Ci, " Method: Brunner-Munzel (BM), Permutation (PERM)" ))}
                box()        
      }   
       
}
