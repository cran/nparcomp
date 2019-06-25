##' Visualizing the result of \code{\link{mctp}}
##' 
##' This function takes an object of class "mctp" and creates a plot of the
##' confidence intervals for the estimated effects.
##' 
##' It is not possible to change any parameter set in the
##' \code{\link{mctp}}-statement.
##' 
##' Since plot.mctp is a S3 method it suffices to use plot(x) as long as x is
##' of class "mctp". It will be interpreted as plot.mctp(x).
##' 
##' @param x An object of class "mctp", i.e. the result when applying
##' \code{\link{mctp}} to a dataset. Otherwise an error will occur.
##' @param ... Arguments to be passed to methods.
##' @return plot.mctp returns a graph that contains a confidence interval for
##' the estimated effect of each contrast. It just visualizes the result of the
##' \code{\link{mctp}}-statement.
##' @note It is possible to create a graphical result of the multiple
##' comparison test procedure directly by setting plot.simci=TRUE in the
##' \code{\link{mctp}}-statement.
##' 
##' To get a complete result summary of \code{\link{mctp}} the function
##' \code{\link{summary.mctp}} can be used.
##' @author Frank Konietschke, Kimihiro Noguchi
##' @seealso For further information on the usage of mctp, see
##' \code{\link{mctp}}.
##' @references F. Konietschke, L.A. Hothorn, E. Brunner: Rank-Based Multiple
##' Test Procedures and Simultaneous Confidence Intervals. Electronic Journal
##' of Statistics, Vol.0 (2011) 1-8.
##' @keywords aplot
##' @examples
##' 
##' data(liver)
##' a<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
##'         type = "Dunnett", alternative = "two.sided", plot.simci = FALSE, 
##'         info = FALSE)
##' plot(a)
##' 
plot.mctp <-
function(x,...)
{
  
	nc <- length(x$connames)
	text.Ci <- paste(x$input$conf.level*100, "%", "Simultaneous Confidence Intervals")
	Lowerp <- "|"
	#updated
	asy.method <- x$input$asy.method
	alternative <- x$input$alternative
	if(asy.method!="log.odds") {
		plot(x$Analysis.Inf$Estimator,1:nc,xlim=c(-1,1), pch=15,axes=FALSE,xlab="",ylab="")
    	points(x$Analysis.Inf$Lower,1:nc, pch=Lowerp,font=2,cex=2)
    	points(x$Analysis.Inf$Upper,1:nc, pch=Lowerp,font=2,cex=2)
    		abline(v=0, lty=3,lwd=2)
    		for (ss in 1:nc){
    			polygon(x=c(x$Analysis.Inf$Lower[ss],x$Analysis.Inf$Upper[ss]),y=c(ss,ss),lwd=2)
    		}
    		axis(1, at = seq(-1, 1, 0.1))
    }
 	else {#log.odds
 		if(alternative=="two.sided") {
 			LowerPlot <- x$Analysis.Inf$Lower
 			UpperPlot <- x$Analysis.Inf$Upper
 		}
 		else if(alternative=="less") {
 			LowerPlot <- x$Analysis.Inf$Estimator - (x$Analysis.Inf$Upper - x$Analysis.Inf$Estimator)
 			UpperPlot <- x$Analysis.Inf$Upper
 		}
 		else { #alternative=="greater"
 			LowerPlot <- x$Analysis.Inf$Lower
 			UpperPlot <- x$Analysis.Inf$Estimator + (x$Analysis.Inf$Estimator - x$Analysis.Inf$Lower)
 		} 
 			
    		plot(x$Analysis.Inf$Estimator, 1:nc, xlim = c(floor(min(LowerPlot)), ceiling(max(UpperPlot))), 
    		pch = 15, axes = FALSE, xlab = "", ylab = "")
    		axis(1, at = seq(floor(min(LowerPlot)), ceiling(max(UpperPlot)), 
    		0.05*(ceiling(max(UpperPlot))-floor(min(LowerPlot)))))          	
    		hugenumber<-10000000
    		if(alternative=="two.sided") {
			points(LowerPlot, 1:nc, pch = Lowerp, font = 2, cex = 2)
        		points(UpperPlot, 1:nc, pch = Lowerp, font = 2, cex = 2)
        		for (ss in 1:nc) {
				polygon(x = c(LowerPlot[ss], UpperPlot[ss]), y = c(ss, ss), lwd = 2)
        		}              		
		}
    		else if(alternative=="less") {
			points(UpperPlot, 1:nc, pch = Lowerp, font = 2, cex = 2)  
			for (ss in 1:nc) {
				polygon(x = c(-hugenumber, UpperPlot[ss]), y = c(ss, ss), lwd = 2)
        		}               		      		
    		}
    		else{ #greater
			points(LowerPlot, 1:nc, pch = Lowerp, font = 2, cex = 2)
			for (ss in 1:nc) {
				polygon(x = c(LowerPlot[ss], hugenumber), y = c(ss, ss), lwd = 2)           		
			}           		         	
		}		
    		abline(v = 0, lty = 3, lwd = 2)   	  			
	}   

 	axis(2,at=1:nc,labels=x$connames)
 	box()	   
 	title(main=c(text.Ci, paste("Type of Contrast:",x$input$type), paste("Method:", x$AsyMethod)))
}
