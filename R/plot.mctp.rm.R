##' Visualizing the result of \code{\link{mctp.rm}}
##' 
##' This function takes an object of class "mctp.rm" and creates a plot of the
##' confidence intervals for the estimated effects.
##' 
##' It is not possible to change any parameter set in the
##' \code{\link{mctp.rm}}-statement.
##' 
##' Since plot.mctp.rm is a S3 method it suffices to use plot(x) as long as x
##' is of class "mctp.rm". It will be interpreted as plot.mctp.rm(x).
##' 
##' @param x An object of class "mctp.rm", i.e. the result when applying
##' \code{\link{mctp.rm}} to a dataset. Otherwise an error will occur.
##' @param ... Arguments to be passed to methods.
##' @return plot.mctp.rm returns a graph that contains a confidence interval
##' for the estimated effect of each contrast. It just visualizes the result of
##' the \code{\link{mctp.rm}}-statement.
##' @note It is possible to create a graphical result of the multiple
##' comparison test procedure directly by setting plot.simci=TRUE in the
##' \code{\link{mctp.rm}}-statement.
##' 
##' To get a complete result summary of \code{\link{mctp.rm}} the function
##' \code{\link{summary.mctp.rm}} can be used.
##' @author Marius Placzek, Kimihiro Noguchi
##' @seealso For further information on the usage of mctp.rm, see
##' \code{\link{mctp.rm}}.
##' @references F. Konietschke, A.C. Bathke, L.A. Hothorn, E. Brunner: Testing
##' and estimation of purely nonparametric effects in repeated measures
##' designs. Computational Statistics and Data Analysis 54 (2010) 1895-1905.
##' @keywords aplot
##' @examples
##' 
##' data(panic)
##' a<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
##'            alternative = "two.sided",
##'            asy.method =  "fisher", contrast.matrix = NULL)
##' plot(a)
##' 
plot.mctp.rm <-
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
