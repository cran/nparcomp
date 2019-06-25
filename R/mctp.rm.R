##' Nonparametric multiple contrast tests and simultaneous confidence intervals
##' (repeated measures)
##' 
##' The function mctp.rm computes the estimator of nonparametric relative
##' effects based on global rankings, simultaneous confidence intervals for the
##' effects, and adjusted p-values based on contrasts in the setting of a
##' repeated measures design with n independent individuals and d repeated
##' measures. Contrasts include "Tukey", "Dunnett", "Sequen", "Williams",
##' "Changepoint", "AVE", "McDermott", "Marcus", "UmbrellaWilliams",
##' "GrandMean", and "UserDefined". The statistics are computed using
##' multivariate normal distribution, multivariate Satterthwaite
##' t-Approximation, and multivariate transformations (adjusted log odds or
##' Fisher function). The function 'mctp.rm' computes both the one-sided and
##' two-sided simultaneous confidence intervals and adjusted p-values. The
##' confidence intervals can be plotted.
##' 
##' 
##' @param formula A two-sided 'formula' specifying a numeric response variable
##' and a repeated measures factor with more than two levels.  If the factor
##' contains less than 3 levels, an error message will be returned.
##' @param data A dataframe containing the variables specified in formula.
##' @param type Character string defining the type of contrast. It should be
##' one of "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE",
##' "McDermott", "Marcus", "UmbrellaWilliams", "GrandMean", "UserDefined".
##' @param conf.level The confidence level for \code{conf.level}-confidence
##' intervals (default is 0.95).
##' @param alternative Character string defining the alternative hypothesis,
##' one of "two.sided", "less", or "greater".
##' @param asy.method Character string defining the asymptotic approximation
##' method, one of "log.odds" (for using the adjusted log odds effect sizes),
##' "mult.t" (for using a multivariate t-distribution with a Satterthwaite
##' Approximation), "fisher" (for using the Fisher transformation function), or
##' "normal" (for using the multivariate normal distribution).
##' @param plot.simci A logical indicating whether you want a plot of the
##' confidence intervals.
##' @param control Character string defining the control group in Dunnett
##' comparisons. By default, it is the first group by definition of the factor
##' variable.
##' @param info A logical whether you want a brief overview with informations
##' about the output.
##' @param rounds Number of rounds for the numeric values of the output
##' (default is 3).
##' @param contrast.matrix User-defined contrast matrix.
##' @param correlation A logical whether the estimated correlation matrix and
##' covariance matrix should be printed.
##' @param const Number used for the adjustment of log odds when the "log.odds"
##' option is chosen.
##' @return
##' 
##' \item{Data.Info}{List of samples and sample sizes and estimated effect per
##' repeated measures level.} 
##' \item{Contrast}{Contrast matrix.}
##' \item{Analysis}{ Estimator: Estimated relative effect, 
##' Lower: Lower limit of the simultaneous confidence intervals, 
##' Upper: Upper limit of the simultaneous confidence intervals, 
##' Statistic: Test statistic 
##' p.Value: Adjusted p-values for the hypothesis by the choosen approximation method.}
##' \item{Analysis.Inf}{The same as \code{Analysis} except that it assumes
##' \code{rounds = Inf}.} 
##' \item{Overall}{The critical value and adjusted p-value for the overall hypothesis.} 
##' \item{input}{List of input arguments by user.} 
##' \item{text.Output}{Character string specifying the alternative hypotheses.} 
##' \item{connames}{Character string specifying the contrast names.} 
##' \item{AsyMethod}{Character string specifying the approximation method.}
##' @note Estimated relative effects with 0 or 1 are replaced with 0.001 and
##' 0.999.
##' 
##' A summary and a graph can be created separately by using the functions
##' \code{\link{summary.mctp.rm}} and \code{\link{plot.mctp.rm}}.
##' 
##' For the analysis, the R packages 'multcomp' and 'mvtnorm' are required.
##' @author Marius Placzek, Kimihiro Noguchi
##' @seealso To analyse simple one-way layouts with independent samples use
##' \code{\link{mctp}}.
##' @references F. Konietschke, A.C. Bathke, L.A. Hothorn, E. Brunner: Testing
##' and estimation of purely nonparametric effects in repeated measures
##' designs. Computational Statistics and Data Analysis 54 (2010) 1895-1905.
##' 
##' Konietschke, F., Placzek, M., Schaarschmidt, S., Hothorn, L.A. (2015).
##' nparcomp: An R Software Package for Nonparametric Multiple Comparisons and
##' Simultaneous Confidence Intervals. Journal of Statistical Software, 61(10),
##' 1-17.
##' 
##' Noguchi, K., Abel, R.S., Marmolejo-Ramos, F., Konietschke, F. (2019).
##' Nonparametric multiple comparisons. Behavior Research Methods,
##' 1-14. DOI:10.3758/s13428-019-01247-9
##' @keywords htest
##' @examples
##' 
##' data(panic)
##' a<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
##'            alternative = "two.sided",
##'            asy.method = "log.odds", plot.simci = FALSE,
##'            info = FALSE, contrast.matrix = NULL)
##' summary(a)
##' plot(a)
##' 
##' b<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
##'            alternative = "two.sided",
##'            asy.method = "mult.t", plot.simci = FALSE,
##'            info = FALSE, contrast.matrix = NULL)
##' summary(b)
##' plot(b)
##' 
##' c<-mctp.rm(CGI~week, data=panic, type = "Dunnett",
##'            alternative = "two.sided",
##'            asy.method = "fisher", plot.simci = FALSE,
##'            info = FALSE, contrast.matrix = NULL)
##' summary(c)
##' plot(c)
##' 
##' d<-mctp.rm(CGI~week, data=panic, type = "Tukey",
##'         alternative = "two.sided",
##'         asy.method = "mult.t", plot.simci = TRUE)
##' summary(d)
##' 
##' @export mctp.rm
mctp.rm <- function (formula, data, type = c("Tukey", "Dunnett", "Sequen", 
	"Williams", "Changepoint", "AVE", "McDermott", "Marcus", 
	"UmbrellaWilliams", "GrandMean", "UserDefined"), conf.level = 0.95, 
    alternative = c("two.sided", "less", "greater"), asy.method = c("log.odds", 
    "fisher", "mult.t", "normal"), plot.simci = FALSE, control = NULL, 
    info = TRUE, rounds = 3, contrast.matrix = NULL, correlation = FALSE, 
    const=1/1.702) 
{
	type <- match.arg(type)
    alternative <- match.arg(alternative)
    asy.method <- match.arg(asy.method)
    input.list <- list(formula = formula, data = data, type = type[1], 
        conf.level = conf.level, alternative = alternative, asy.method = asy.method, 
        plot.simci = plot.simci, control = control, info = info, 
        rounds = rounds, contrast.matrix = contrast.matrix, correlation = correlation, const=const)
    conflevel <- conf.level
    if (conflevel >= 1 || conflevel <= 0) {
        stop("The confidence level must be between 0 and 1!")
        if (is.null(alternative)) {
            stop("Please declare the alternative! (two.sided, less, greater)")
        }
    }
    if (length(formula) != 3) {
        stop("You can only analyse one-way layouts!")
    }
    dat <- model.frame(formula, data)
    if (ncol(dat) != 2) {
        stop("Specify one response and only one class variable in the formula")
    }
    if (is.numeric(dat[, 1]) == FALSE) {
        stop("Response variable must be numeric")
    }
    response <- dat[, 1]
    factorx <- as.factor(dat[, 2])
    fl <- levels(factorx)
    a <- nlevels(factorx)
    if (a <= 2) {
        stop("You want to perform a two-sample test. Please use the function npar.t.test.paired")
    }
    samples <- split(response, factorx)
    n <- as.numeric(sapply(samples, length))
    if (any(n == 1)) {
        warn <- paste("The factor level", fl[n == 1], "has got only one observation!")
        stop(warn)
    }
    a <- length(n)
    if (type == "UserDefined") {
        if (is.null(contrast.matrix)) {
            stop("Please enter a contrast matrix!")
        }
        ch <- contrast.matrix
        rownames(ch) <- paste("C", 1:nrow(ch))
        colnames(ch) <- fl
    }
    if (type != "UserDefined") {
        if (is.null(control)) {
            icon <- 1
        }
        if (!is.null(control)) {
            icon <- which(fl == control)
        }
        ch <- contrMat(n = n, type, base = icon)
    }
    nc <- nrow(ch)
    connames <- rownames(ch)
    Con <- matrix(ch, ncol = a)
    rownames(Con) <- connames
    colnames(Con) <- colnames(ch)
    
    #Standardizing contrasts
    if(asy.method == "log.odds") {
    Con <- 2*Con/rowSums(abs(Con))}    
    
    tmp1 <- sort(rep(1:a, a))
    tmp2 <- rep(1:a, a)
    pairRanks <- lapply(1:(a^2), function(arg) rank(c(samples[[tmp1[arg]]], 
        samples[[tmp2[arg]]])))
    intRanks <- lapply(1:a, function(arg) rank(samples[[arg]]))
    n <- n[[1]]
    plis <- lapply(1:(a^2), function(arg) 1/n * (mean(pairRanks[[arg]][(n + 
        1):(2 * n)]) - (n + 1)/2))
    vec.plis <- as.numeric(as.character(plis))
    pd <- c()
    for (i in 1:a) {
        pd[i] <- 1/a * sum((tmp2 == i) * vec.plis)
    }
    pd1 <- (pd == 1)
    pd0 <- (pd == 0)
    pd[pd1] <- 0.999
    pd[pd0] <- 0.001
    Zlong <- c()
    for (i in 1:(a^2)) {
        Zlong <- cbind(Zlong, 1/n * (pairRanks[[i]][1:n] - intRanks[[tmp1[i]]] - 
            pairRanks[[i]][(n + 1):(2 * n)] + intRanks[[tmp2[i]]]))
    }
    Zquer <- 1/n * colSums(Zlong)
    Yd <- matrix(rep(0, a^4), nrow = a^2, ncol = a^2)
    for (k in 1:n) {
        Yd <- Yd + 1/(n - 1) * (t(Zlong[k, ] - t(Zquer)) %*% 
            (Zlong[k, ] - t(Zquer)))
    }
    W <- matrix(rep(1/a * diag(a), a), nrow = a)
    sYd <- svd(Yd)
    dYd <- diag(sqrt(sYd$d))
    hYd <- sYd$u %*% dYd %*% t(sYd$v)    
    hVd <- W %*% hYd
    Vd <- hVd %*% t(hVd)
    logit.pd <- log(c(pd/(1 - pd)))
    logit.pd.dev <- diag(1/c((pd * (1 - pd))))
    hlVd <- logit.pd.dev %*% hVd
    lVd <- hlVd %*% t(hlVd)       
    Lower.logit1 <- logit.pd - qnorm(conf.level)/sqrt(n) * sqrt(c(diag(lVd)))
    Upper.logit1 <- logit.pd + qnorm(conf.level)/sqrt(n) * sqrt(c(diag(lVd)))
    Lower.logit <- exp(Lower.logit1)/(1 + exp(Lower.logit1))
    Upper.logit <- exp(Upper.logit1)/(1 + exp(Upper.logit1))
    corr.mat <- function(m, nc) {
        rho <- matrix(c(0), ncol = nc, nrow = nc)
        for (i in 1:nc) {
            for (j in 1:nc) {
                rho[i, j] <- m[i, j]/sqrt(m[i, i] * m[j, j])
            }
        }
        return(rho)
    }
    dfT <- n - 1
    ind.plus <- matrix(as.integer(Con > 0), ncol=ncol(Con),nrow=nrow(Con))
    ind.minus <- matrix(as.integer(Con < 0), ncol=ncol(Con),nrow=nrow(Con))
    Con.plus <- Con*ind.plus
    Con.minus<- -Con*ind.minus
     
    Cpd.plus <- Con.plus %*% pd	
    Cpd.minus <- Con.minus %*% pd       
    Cpd <- Con %*% pd
    hCV <- Con %*% hVd
    CV <- hCV %*% t(hCV)
    rhobf <- corr.mat(CV, nc)
    p.adj <- c()
    switch(asy.method, mult.t = {
        Tstat <- sqrt(n) * (Cpd)/sqrt(c(diag(CV)))
        AsyMethod <- paste("Multi - T with", round(dfT, rounds), 
            "DF")
        switch(alternative, two.sided = {
            text.Output <- paste("True differences of relative effects are not equal to 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -abs(Tstat[pp]), upper = abs(Tstat[pp]), 
                  delta = rep(0, nc), df = dfT, corr = rhobf)[1]
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "both", 
                df = dfT)$quantile
            Lower <- Cpd - crit/sqrt(n) * sqrt(c(diag(CV)))
            Upper <- Cpd + crit/sqrt(n) * sqrt(c(diag(CV)))
        }, less = {
            text.Output <- paste("True differences of relative effects are less than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = Tstat[pp], upper = Inf, 
                  df = dfT, delta = rep(0, nc), corr = rhobf)
            }
            crit <- qmvt(conflevel, df = dfT, corr = rhobf, tail = "lower")$quantile
            Lower <- rep(-1, nc)
            Upper <- Cpd + crit/sqrt(n) * sqrt(c(diag(CV)))
        }, greater = {
            text.Output <- paste("True differences of relative effects are greater than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -Inf, upper = Tstat[pp], 
                  df = dfT, delta = rep(0, nc), corr = rhobf)[1]
            }
            crit <- qmvt(conflevel, corr = rhobf, df = dfT, tail = "lower")$quantile
            Lower <- Cpd - crit/sqrt(n) * sqrt(c(diag(CV)))
            Upper <- rep(1, nc)
        })
    }, normal = {
        AsyMethod <- "Normal - Approximation"
        Tstat <- sqrt(n) * (Cpd)/sqrt(c(diag(CV)))
        switch(alternative, two.sided = {
            text.Output <- paste("True differences of relative effects are not equal to 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvnorm(lower = -abs(Tstat[pp]), 
                  upper = abs(Tstat[pp]), mean = rep(0, nc), corr = rhobf)[1]
            }
            crit <- qmvnorm(conflevel, corr = rhobf, tail = "both")$quantile
            Lower <- Cpd - crit/sqrt(n) * sqrt(c(diag(CV)))
            Upper <- Cpd + crit/sqrt(n) * sqrt(c(diag(CV)))
        }, less = {
            text.Output <- paste("True differences of relative effects are less than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvnorm(lower = Tstat[pp], upper = Inf, 
                  mean = rep(0, nc), corr = rhobf)
            }
            crit <- qmvnorm(conflevel, corr = rhobf, tail = "lower")$quantile
            Lower <- rep(-1, nc)
            Upper <- Cpd + crit/sqrt(n) * sqrt(c(diag(CV)))
        }, greater = {
            text.Output <- paste("True differences of relative effects are greater than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvnorm(lower = -Inf, upper = Tstat[pp], 
                  mean = rep(0, nc), corr = rhobf)
            }
            crit <- qmvnorm(conflevel, corr = rhobf, tail = "lower")$quantile
            Lower <- Cpd - crit/sqrt(n) * sqrt(c(diag(CV)))
            Upper <- rep(1, nc)
        })
    }, fisher = {
        AsyMethod <- paste("Fisher with", round(dfT, rounds), 
            "DF")
        Cfisher <- 1/2 * log((1 + Cpd)/(1 - Cpd))
        Vfisherdev <- diag(c(1/(1 - Cpd^2)))   
        hVfisher <- Vfisherdev %*% hCV
        Vfisher <- hVfisher %*% t(hVfisher)
        Tstat <- sqrt(n) * Cfisher/sqrt(c(diag(Vfisher)))
        switch(alternative, two.sided = {
            text.Output <- paste("True differences of relative effects are not equal to 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -abs(Tstat[pp]), upper = abs(Tstat[pp]), 
                  delta = rep(0, nc), corr = rhobf, df = dfT)[1]
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "both", 
                df = dfT)$quantile
            Lower1 <- Cfisher - crit/sqrt(n) * sqrt(c(diag(Vfisher)))
            Upper1 <- Cfisher + crit/sqrt(n) * sqrt(c(diag(Vfisher)))
            Lower <- (exp(2 * Lower1) - 1)/(exp(2 * Lower1) + 
                1)
            Upper <- (exp(2 * Upper1) - 1)/(exp(2 * Upper1) + 
                1)
        }, less = {
            text.Output <- paste("True differences of relative effects are less than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- pmvt(lower = Tstat[pp], upper = Inf, 
                  delta = rep(0, nc), df = dfT, corr = rhobf)
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "lower", 
                df = dfT)$quantile
            Lower <- rep(-1, nc)
            Upper1 <- Cfisher + crit/sqrt(n) * sqrt(c(diag(Vfisher)))
            Upper <- (exp(2 * Upper1) - 1)/(exp(2 * Upper1) + 
                1)
        }, greater = {
            text.Output <- paste("True differences of relative effects are greater than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -Inf, upper = Tstat[pp], 
                  delta = rep(0, nc), corr = rhobf, df = dfT)
            }
            crit <- qmvnorm(conflevel, corr = rhobf, tail = "lower")$quantile
            Lower1 <- Cfisher - crit/sqrt(n) * sqrt(c(diag(Vfisher)))
            Lower <- (exp(2 * Lower1) - 1)/(exp(2 * Lower1) + 
                1)
            Upper <- rep(1, nc)
        })
    }, log.odds = {
       	AsyMethod <- paste("Adjusted log odds with", round(dfT, rounds), "DF")
        Clogodds <- const * (log(Cpd.plus/(1-Cpd.plus)) - log(Cpd.minus/(1-Cpd.minus)))
        glogodds.plus <- Cpd.plus - Cpd.plus^2
        glogodds.minus <- Cpd.minus - Cpd.minus^2
        glogodds.plus.mat <- matrix(1,nrow=1,ncol=a) %x% matrix(glogodds.plus, ncol=1)
        glogodds.minus.mat <- matrix(1,nrow=1,ncol=a) %x% matrix(glogodds.minus, ncol=1)     
        Glogodds <- const * (Con.plus/glogodds.plus.mat - Con.minus/glogodds.minus.mat)
        	
        hVlogodds <- Glogodds %*% hVd
        Vlogodds <- hVlogodds %*% t(hVlogodds)
        Tstat <- sqrt(n) * Clogodds/sqrt(c(diag(Vlogodds)))
        switch(alternative, two.sided = {
            text.Output <- paste("True differences of relative effects are not equal to 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -abs(Tstat[pp]), upper = abs(Tstat[pp]), 
                  delta = rep(0, nc), corr = rhobf, df = dfT)[1]
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "both", 
                df = dfT)$quantile
            Lower <- Clogodds - crit/sqrt(n) * sqrt(c(diag(Vlogodds)))
            Upper <- Clogodds + crit/sqrt(n) * sqrt(c(diag(Vlogodds)))
            LowerPlot <- Lower
            UpperPlot <- Upper
        }, less = {
            text.Output <- paste("True differences of relative effects are less than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = Tstat[pp], upper = Inf, 
                  delta = rep(0, nc), df = dfT, corr = rhobf)
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "lower", 
                df = dfT)$quantile
            Lower <- rep(-Inf, nc)
            LowerPlot <- Clogodds - crit/sqrt(n) * sqrt(c(diag(Vlogodds)))
            Upper <- Clogodds + crit/sqrt(n) * sqrt(c(diag(Vlogodds)))
            UpperPlot <- Upper
        }, greater = {
            text.Output <- paste("True differences of relative effects are greater than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -Inf, upper = Tstat[pp], 
                  delta = rep(0, nc), corr = rhobf, df = dfT)
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "lower")$quantile
            Lower <- Clogodds - crit/sqrt(n) * sqrt(c(diag(Vlogodds)))
            LowerPlot <- Lower
            Upper <- rep(Inf, nc)
            UpperPlot <- Clogodds + crit/sqrt(n) * sqrt(c(diag(Vlogodds)))
        })
    })
    
    if (plot.simci == TRUE) {
        text.Ci <- paste(conflevel * 100, "%", "Simultaneous Confidence Intervals")
        Lowerp <- "|"
        
        if(asy.method!="log.odds"){        
        		plot(Cpd, 1:nc, xlim = c(-1, 1), pch = 15, axes = FALSE, 
            xlab = "", ylab = "")
            axis(1, at = seq(-1, 1, 0.1))
        	points(Lower, 1:nc, pch = Lowerp, font = 2, cex = 2)
        	points(Upper, 1:nc, pch = Lowerp, font = 2, cex = 2)
        	abline(v = 0, lty = 3, lwd = 2)
        	for (ss in 1:nc) {
            	polygon(x = c(Lower[ss], Upper[ss]), y = c(ss, ss), lwd = 2)
        	}            
        }
        else{ #log.odds
        	plot(Clogodds, 1:nc, xlim = c(floor(min(LowerPlot)), ceiling(max(UpperPlot))), pch = 15, axes = FALSE, 
        	xlab = "", ylab = "")
            axis(1, at = seq(floor(min(LowerPlot)), ceiling(max(UpperPlot)), 0.1*(ceiling(max(UpperPlot))-floor(min(LowerPlot)))))          	
        	hugenumber<-10000000
            if(alternative=="two.sided"){
        		points(LowerPlot, 1:nc, pch = Lowerp, font = 2, cex = 2)
        		points(UpperPlot, 1:nc, pch = Lowerp, font = 2, cex = 2)
        		for (ss in 1:nc) {
            		polygon(x = c(LowerPlot[ss], UpperPlot[ss]), y = c(ss, ss), lwd = 2)
        		}              		
        	}
        	else if(alternative=="less"){
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
        axis(2, at = 1:nc, labels = connames)
        box()
        title(main = c(text.Ci, paste("Type of Contrast:", type), 
            paste("Method:", AsyMethod)))
    }
    data.info <- data.frame(Sample = fl, Size = n, Effect = pd, 
        Lower = Lower.logit, Upper = Upper.logit)
    if(asy.method!="log.odds"){
    	 est<-Cpd
    }
    else{
    	 est<-Clogodds
    }            
    Analysis.of.Relative.Effects <- data.frame(Estimator = round(est, 
        rounds), Lower = round(Lower, rounds), Upper = round(Upper, 
        rounds), Statistic = round(Tstat, rounds), p.Value = p.adj)
    Analysis.Inf <- data.frame(Estimator = est, Lower = Lower, Upper = Upper, 
    	Statistic = Tstat, p.Value = p.adj)     
    Overall <- data.frame(Quantile = crit, p.Value = min(p.adj))
    result <- list(Data.Info = data.info, Contrast = Con, Analysis = Analysis.of.Relative.Effects, 
    		Analysis.Inf = Analysis.Inf, Overall = Overall)

    if (info == TRUE) {
        cat("\n", "#----------------Nonparametric Multiple Comparisons for relative effects---------------#", 
            "\n", "\n", "-", "Alternative Hypothesis: ", text.Output, 
            "\n", "-", "Estimation Method: Global Pseudo ranks", 
            "\n", "-", "Type of Contrast", ":", type, "\n", "-", 
            "Confidence Level:", conflevel * 100, "%", "\n", 
            "-", "Method", "=", AsyMethod, "\n", "\n", "#--------------------------------------------------------------------------------------#", 
            "\n", "\n")
    }
    if (correlation == TRUE) {
        result$Covariance <- CV
        result$Correlation <- rhobf
    }
    result$input <- input.list
    result$text.Output <- text.Output
    result$connames <- connames
    result$AsyMethod <- AsyMethod
    class(result) <- "mctp.rm"
    return(result)
}
