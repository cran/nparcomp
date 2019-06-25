##' Nonparametric multiple contrast tests and simultaneous confidence intervals
##' (independent samples)
##' 
##' The function mctp computes the estimator of nonparametric relative effects
##' based on global rankings, simultaneous confidence intervals for the
##' effects, and adjusted p-values based on contrasts in the setting of
##' independent samples. Contrasts include "Tukey", "Dunnett", "Sequen",
##' "Williams", "Changepoint", "AVE", "McDermott", "Marcus",
##' "UmbrellaWilliams", "GrandMean", and "UserDefined". The statistics are
##' computed using multivariate normal distribution, multivariate Satterthwaite
##' t-Approximation, and multivariate transformations (adjusted log odds or
##' Fisher function). The function 'mctp' computes both the one-sided and
##' two-sided simultaneous confidence intervals and adjusted p-values. The
##' simultaneous confidence intervals can be plotted.
##' 
##' 
##' @param formula A two-sided 'formula' specifying a numeric response variable
##' and a factor with more than two levels. If the factor contains less than 3
##' levels, an error message will be returned.
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
##' @param effect Character string defining the type of effect, one of
##' "unweighted" and "weighted".
##' @param const Number used for the adjustment of log odds when the "log.odds"
##' option is chosen.
##' @return
##' 
##' \item{Data.Info}{List of samples and sample sizes and estimated effect per
##' group.} 
##' \item{Contrast}{Contrast matrix.} 
##' \item{Analysis}{ Estimator: Estimated relative effect, 
##' Lower: Lower limit of the simultaneous confidence interval, 
##' Upper: Upper limit of the simultaneous confidence interval, 
##' Statistic: Test statistic p.Value: Adjusted p-values for the
##' hypothesis by the choosen approximation method.} 
##' \item{Analysis.Inf}{The same as \code{Analysis} except that it assumes 
##' \code{rounds = Inf}.}
##' \item{Overall}{The critical value and adjusted p-value for the overall
##' hypothesis.} 
##' \item{input}{List of input arguments by user.}
##' \item{text.Output}{Character string specifying the alternative hypotheses.}
##' \item{text.output.W}{Character string specifying the weight pattern for the
##' reference distribution.} 
##' \item{connames}{Character string specifying the contrast names.} 
##' \item{AsyMethod}{Character string specifying the approximation method.}
##' @note If the samples are completely seperated the variance estimators are
##' Zero by construction. In these cases the Null-estimators are replaced by
##' 0.001. Estimated relative effects with 0 or 1 are replaced with 0.001,
##' 0.999 respectively.
##' 
##' A summary and a graph can be created separately by using the functions
##' \code{\link{summary.mctp}} and \code{\link{plot.mctp}}.
##' 
##' For the analysis, the R packages 'multcomp' and 'mvtnorm' are required.
##' @author Frank Konietschke, Kimihiro Noguchi
##' @seealso For simultaneous confidence intervals for relative contrast
##' effects, see \code{\link{nparcomp}}.
##' @references F. Konietschke, L.A. Hothorn, E. Brunner: Rank-Based Multiple
##' Test Procedures and Simultaneous Confidence Intervals. Electronic Journal
##' of Statistics, Vol.0 (2011) 1-8.
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
##' 
##' data(liver)
##' 
##'   # Williams Contrast
##' 
##' a<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
##'         type = "Williams", alternative = "two.sided", 
##'         plot.simci = TRUE, info = FALSE)
##' summary(a)
##' 
##'  # Dunnett Contrast
##' 
##' b<-mctp(weight ~dosage, data=liver, asy.method = "fisher",
##'         type = "Dunnett", alternative = "two.sided", 
##'         plot.simci = TRUE, info = FALSE)
##' summary(b)
##' 
##'  # Dunnett dose 3 is baseline
##' 
##' c<-mctp(weight ~dosage, data=liver, asy.method = "log.odds",
##'         type = "Dunnett", control = "3",alternative = "two.sided",
##'         plot.simci = TRUE, info = FALSE)
##' summary(c)
##' 
##' 
##' data(colu)
##' 
##'   # Tukey comparison- one sided (less)
##' 
##' a<-mctp(corpora~ dose, data=colu, asy.method = "log.odds",
##'         type = "Tukey",alternative = "less", 
##'         plot.simci = TRUE, info = FALSE)
##' summary(a)
##' 
##'  # Tukey comparison- one sided (greater)
##' 
##' b<-mctp(corpora~ dose, data=colu, asy.method = "mult.t",
##'         type = "Tukey",alternative = "greater", 
##'         plot.simci = TRUE, info = FALSE)
##' summary(b)
##' 
##'   # Tukey comparison- one sided (less)
##' 
##' c<-mctp(corpora~ dose, data=colu, asy.method = "mult.t",
##'         type = "Tukey",alternative = "less", 
##'         plot.simci = TRUE, info = FALSE)
##' summary(c)
##' 
##'  # Marcus comparison- one sided (greater)
##' 
##' d<-mctp(corpora~ dose, data=colu, asy.method = "fisher",
##'         type = "Marcus",alternative = "greater", 
##'         plot.simci = TRUE, info = FALSE)
##' summary(d)
##' 
##' @export mctp
mctp <- function (formula, data, type = c("Tukey", "Dunnett", "Sequen", 
    "Williams", "Changepoint", "AVE", "McDermott", "Marcus", 
    "UmbrellaWilliams", "GrandMean", "UserDefined"), conf.level = 0.95, 
    alternative = c("two.sided", "less", "greater"), asy.method = c("fisher", "mult.t", "normal","log.odds"), 
    plot.simci = FALSE, control = NULL, 
    info = TRUE, rounds = 3, contrast.matrix = NULL, correlation = FALSE, 
    effect = c("unweighted", "weighted"), const=1/1.702) 
    #updated to add "GrandMean" and const
{
    type <- match.arg(type)
    alternative <- match.arg(alternative)
    asy.method <- match.arg(asy.method)
    effect <- match.arg(effect)	
    input.list <- list(formula = formula, data = data, type = type[1], 
        conf.level = conf.level, alternative = alternative, asy.method = asy.method, 
        plot.simci = plot.simci, control = control, info = info, 
        rounds = rounds, contrast.matrix = contrast.matrix, correlation = correlation, 
        effect = effect, const=const)
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
    samples <- split(response, factorx)
    fl <- levels(factorx)
    a <- nlevels(factorx)
    if (a <= 2) {
        stop("You want to perform a two-sample test. Please use the function npar.t.test")
    }
    n <- as.numeric(sapply(samples, length)) #updated for better stability
    if (any(n <= 1)) {
        warn <- paste("The factor level", fl[n <= 1], "has got only one observation!")
        stop(warn)
    }
    N <- sum(n)
    tmp1 <- sort(rep(1:a, a))
    tmp2 <- rep(1:a, a)
    pairRanks <- lapply(1:(a^2), function(arg) rank(c(samples[[tmp1[arg]]], 
        samples[[tmp2[arg]]])))
    p <- sapply(1:(a^2), function(arg) {
        x1 <- samples[[tmp1[arg]]]
        x2 <- samples[[tmp2[arg]]]
        rx1x2 <- rank(c(x1, x2))
        l1 <- length(x1)
        l2 <- length(x2)
        1/(l1 + l2) * (mean(rx1x2[(l1 + 1):(l1 + l2)]) - mean(rx1x2[1:l1])) + 
            0.5
    })
    intRanks <- lapply(samples, rank)
    placements <- lapply(1:(a^2), function(arg) 1/n[tmp1[arg]] * 
        (pairRanks[[arg]][(n[tmp1[arg]] + 1):(n[tmp1[arg]] + 
            n[tmp2[arg]])] - intRanks[[tmp2[arg]]]))
    V <- rep(0, a^4)
    help <- expand.grid(1:a, 1:a, 1:a, 1:a)
    h1 <- help[, 4]
    h2 <- help[, 3]
    h3 <- help[, 2]
    h4 <- help[, 1]
    for (u in 1:(a^4)) {
        i <- h1[u]
        j <- h2[u]
        r <- h3[u]
        s <- h4[u]
        if (i == r && j == s && i != j && r != s) {
            xi <- samples[[i]]
            xj <- samples[[j]]
            ni <- length(xi)
            nj <- length(xj)
            ri <- rank(xi)
            rj <- rank(xj)
            rij <- rank(c(xi, xj))
            pj <- 1/ni * (rij[(ni + 1):(ni + nj)] - rj)
            pi <- 1/nj * (rij[1:ni] - ri)
            vi <- var(pi)/ni
            vj <- var(pj)/nj
            V[u] <- N * (vi + vj)
        }
        if (i == s && j == r && i != j && r != s) {
            xi <- samples[[i]]
            xj <- samples[[j]]
            ni <- length(xi)
            nj <- length(xj)
            ri <- rank(xi)
            rj <- rank(xj)
            rij <- rank(c(xi, xj))
            pj <- 1/ni * (rij[(ni + 1):(ni + nj)] - rj)
            pi <- 1/nj * (rij[1:ni] - ri)
            vi <- var(pi)/ni
            vj <- var(pj)/nj
            V[u] <- -N * (vi + vj)
        }
        if (i == r && j != s && i != j && r != s) {
            xi <- samples[[i]]
            xj <- samples[[j]]
            xs <- samples[[s]]
            ni <- length(xi)
            nj <- length(xj)
            ns <- length(xs)
            ri <- rank(xi)
            rj <- rank(xj)
            rs <- rank(xs)
            rij <- rank(c(xi, xj))
            ris <- rank(c(xi, xs))
            pij <- 1/nj * (rij[1:ni] - ri)
            pis <- 1/ns * (ris[1:ni] - ri)
            V[u] <- N * (cov(pij, pis)/ni)
        }
        if (i != r && j == s && i != j && r != s) {
            xi <- samples[[i]]
            xj <- samples[[j]]
            xr <- samples[[r]]
            ni <- length(xi)
            nj <- length(xj)
            nr <- length(xr)
            ri <- rank(xi)
            rj <- rank(xj)
            rr <- rank(xr)
            rji <- rank(c(xj, xi))
            rjr <- rank(c(xj, xr))
            pji <- 1/ni * (rji[1:nj] - rj)
            prj <- 1/nr * (rjr[1:nj] - rj)
            V[u] <- N * (cov(pji, prj)/nj)
        }
        if (i == s && j != r && i != j && r != s) {
            xi <- samples[[i]]
            xj <- samples[[j]]
            xr <- samples[[r]]
            ni <- length(xi)
            nj <- length(xj)
            nr <- length(xr)
            ri <- rank(xi)
            rj <- rank(xj)
            rr <- rank(xr)
            rij <- rank(c(xi, xj))
            rir <- rank(c(xi, xr))
            pij <- 1/nj * (rij[1:ni] - ri)
            pir <- 1/nr * (rir[1:ni] - ri)
            V[u] <- -N * (cov(pij, pir)/ni)
        }
        if (i != s && j == r && i != j && r != s) {
            xi <- samples[[i]]
            xj <- samples[[j]]
            xs <- samples[[s]]
            ni <- length(xi)
            nj <- length(xj)
            ns <- length(xs)
            ri <- rank(xi)
            rj <- rank(xj)
            rs <- rank(xs)
            rji <- rank(c(xj, xi))
            rjs <- rank(c(xj, xs))
            pji <- 1/ni * (rji[1:nj] - rj)
            pjs <- 1/ns * (rjs[1:nj] - rj)
            V[u] <- -N * (cov(pji, pjs)/nj)
        }
    }
    V1 <- matrix(V, ncol = a^2, nrow = a^2)
    switch(effect, weighted = {
        W <- kronecker(t(n/N), diag(a))
        text.output.W <- paste("Global Ranks")
    }, unweighted = {
        W <- kronecker(t(rep(1/a, a)), diag(a))
        text.output.W <- paste("Global Pseudo Ranks")
    })
    pd <- W %*% p
    #updated to avoid non-positive-semidefinte matrix.
    sV1 <- svd(V1)
    dV1 <- diag(sqrt(sV1$d))
    hV1 <- sV1$u %*% dV1 %*% t(sV1$v)
    hVV <- W %*% hV1
    VV <- hVV %*% t(hVV)
    logit.pd <- log(c(pd/(1 - pd)))
    logit.pd.dev <- diag(1/c((pd * (1 - pd))))
    #updated to avoid non-positive-semidefinte matrix.    
    hlVV <- logit.pd.dev %*% hVV
    lVV <- hlVV %*% t(hlVV)    
    Lower.logit1 <- logit.pd - qnorm(conf.level)/sqrt(N) * sqrt(c(diag(lVV)))
    Upper.logit1 <- logit.pd + qnorm(conf.level)/sqrt(N) * sqrt(c(diag(lVV)))
    Lower.logit <- exp(Lower.logit1)/(1 + exp(Lower.logit1))
    Upper.logit <- exp(Upper.logit1)/(1 + exp(Upper.logit1))
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
    
    #updated
    #Standardizing contrasts
    if(asy.method == "log.odds") {
    Con <- 2*Con/rowSums(abs(Con))}
    
    degrees <- function(CC) {
        nc <- nrow(CC)
        
        ind.plus <- matrix(as.integer(CC > 0), ncol=ncol(CC),nrow=nrow(CC))
        ind.minus <- matrix(as.integer(CC < 0), ncol=ncol(CC),nrow=nrow(CC))
        CC.plus <- CC*ind.plus
     	CC.minus<- -CC*ind.minus
    	 
    	Cpd.plus <- CC.plus %*% pd	
     	Cpd.minus <- CC.minus %*% pd	
     	gdcp.plus <- const/(Cpd.plus - Cpd.plus^2)
     	gdcp.minus <- const/(Cpd.minus - Cpd.minus^2)          

     	if(asy.method != "log.odds"){
      		gdcp.plus <- gdcp.plus*0 + 1
      		gdcp.minus <- gdcp.minus*0 + 1
      	}        
                 
        dfs <- c()
        for (hhh in 1:nc) {
            #updated
      		ccg <- gdcp.plus[hhh,]*CC.plus[hhh,]-gdcp.minus[hhh,]*CC.minus[hhh,]            
            Yk <- list()
            for (l in 1:a) {
                Yk[[l]] <- 0
                for (i in 1:(a^2)) {
                  r <- tmp1[i]
                  s <- tmp2[i]
                  if (s == l && r != l) {
                    Ykhelp <- placements[[i]]
                    Yk[[l]] <- Yk[[l]] + Ykhelp
                  }
                }
            }
            Ykstern <- list()
            for (l in 1:a) {
                Ykstern[[l]] <- ccg[l] * Yk[[l]] #updated
                for (i in 1:(a^2)) {
                  r <- tmp1[i]
                  s <- tmp2[i]
                  if (s == l && r != l) {
                    Yksternhelp <- -ccg[r] * placements[[i]] #updated
                    Ykstern[[l]] <- Ykstern[[l]] + Yksternhelp
                  }
                }
            }
            variances <- sapply(Ykstern, var)/(a * n)
            varii2 <- (variances == 0)
            variances[varii2] <- 1/N
            dfs[hhh] <- (sum(variances))^2/sum(variances^2/(n - 
                1))
        }
        dfs
    }
    dfT <- round(max(1, min(degrees(Con)))) #updated

    #updated for positive and negative parts of contrasts for log.odds.
    ind.plus <- matrix(as.integer(Con > 0), ncol=ncol(Con),nrow=nrow(Con))
    ind.minus <- matrix(as.integer(Con < 0), ncol=ncol(Con),nrow=nrow(Con))
    Con.plus <- Con*ind.plus
    Con.minus<- -Con*ind.minus
     
    Cpd.plus <- Con.plus %*% pd	
    Cpd.minus <- Con.minus %*% pd       
    Cpd <- Con %*% pd
    #updated to avoid non-positive-semidefinte matrix.
    hCV <- Con %*% hVV
    CV <- hCV %*% t(hCV)
    rhobf <- cov2cor(CV)
    p.adj <- c()
    switch(asy.method, mult.t = {
        Tstat <- sqrt(N) * (Cpd)/sqrt(c(diag(CV)))
        AsyMethod <- paste("Multi - T with", round(dfT, rounds), 
            "DF")
        switch(alternative, two.sided = {
            text.Output <- paste("True differences of relative effects are not equal to 0") #updated
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -abs(Tstat[pp]), upper = abs(Tstat[pp]), 
                  delta = rep(0, nc), df = dfT, corr = rhobf)[1]
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "both", 
                df = dfT)$quantile
            Lower <- Cpd - crit/sqrt(N) * sqrt(c(diag(CV)))
            Upper <- Cpd + crit/sqrt(N) * sqrt(c(diag(CV)))
        }, less = {
            text.Output <- paste("True differences of relative effects are less than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = Tstat[pp], upper = Inf, 
                  df = dfT, delta = rep(0, nc), corr = rhobf)
            }
            crit <- qmvt(conflevel, df = dfT, corr = rhobf, tail = "lower")$quantile
            Lower <- rep(-1, nc)
            Upper <- Cpd + crit/sqrt(N) * sqrt(c(diag(CV)))
        }, greater = {
            text.Output <- paste("True differences of relative effects are greater than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -Inf, upper = Tstat[pp], 
                  df = dfT, delta = rep(0, nc), corr = rhobf)[1]
            }
            crit <- qmvt(conflevel, corr = rhobf, df = dfT, tail = "lower")$quantile
            Lower <- Cpd - crit/sqrt(N) * sqrt(c(diag(CV)))
            Upper <- rep(1, nc)
        })
    }, normal = {
        AsyMethod <- "Normal - Approximation"
        Tstat <- sqrt(N) * (Cpd)/sqrt(c(diag(CV)))
        switch(alternative, two.sided = {
            text.Output <- paste("True differences of relative effects are not equal to 0") #updated
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvnorm(lower = -abs(Tstat[pp]), 
                  upper = abs(Tstat[pp]), mean = rep(0, nc), corr = rhobf)[1]
            }
            crit <- qmvnorm(conflevel, corr = rhobf, tail = "both")$quantile
            Lower <- Cpd - crit/sqrt(N) * sqrt(c(diag(CV)))
            Upper <- Cpd + crit/sqrt(N) * sqrt(c(diag(CV)))
        }, less = {
            text.Output <- paste("True differences of relative effects are less than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvnorm(lower = Tstat[pp], upper = Inf, 
                  mean = rep(0, nc), corr = rhobf)
            }
            crit <- qmvnorm(conflevel, corr = rhobf, tail = "lower")$quantile
            Lower <- rep(-1, nc)
            Upper <- Cpd + crit/sqrt(N) * sqrt(c(diag(CV)))
        }, greater = {
            text.Output <- paste("True differences of relative effects are greater than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvnorm(lower = -Inf, upper = Tstat[pp], 
                  mean = rep(0, nc), corr = rhobf)
            }
            crit <- qmvnorm(conflevel, corr = rhobf, tail = "lower")$quantile
            Lower <- Cpd - crit/sqrt(N) * sqrt(c(diag(CV)))
            Upper <- rep(1, nc)
        })
    }, fisher = {
        AsyMethod <- paste("Fisher with", round(dfT, rounds), 
            "DF")
        Cfisher <- 1/2 * log((1 + Cpd)/(1 - Cpd))
        Vfisherdev <- diag(c(1/(1 - Cpd^2)))
    	#updated to avoid non-positive-semidefinte matrix.        
        hVfisher <- Vfisherdev %*% hCV
        Vfisher <- hVfisher %*% t(hVfisher)
        Tstat <- sqrt(N) * Cfisher/sqrt(c(diag(Vfisher)))
        switch(alternative, two.sided = {
            text.Output <- paste("True differences of relative effects are not equal to 0") #updated
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -abs(Tstat[pp]), upper = abs(Tstat[pp]), 
                  delta = rep(0, nc), corr = rhobf, df = dfT)[1]
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "both", 
                df = dfT)$quantile
            Lower1 <- Cfisher - crit/sqrt(N) * sqrt(c(diag(Vfisher)))
            Upper1 <- Cfisher + crit/sqrt(N) * sqrt(c(diag(Vfisher)))
            Lower <- (exp(2 * Lower1) - 1)/(exp(2 * Lower1) + 
                1)
            Upper <- (exp(2 * Upper1) - 1)/(exp(2 * Upper1) + 
                1)
        }, less = {
            text.Output <- paste("True differences of relative effects are less than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = Tstat[pp], upper = Inf, 
                  delta = rep(0, nc), df = dfT, corr = rhobf)
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "lower", 
                df = dfT)$quantile
            Lower <- rep(-1, nc)
            Upper1 <- Cfisher + crit/sqrt(N) * sqrt(c(diag(Vfisher)))
            Upper <- (exp(2 * Upper1) - 1)/(exp(2 * Upper1) + 
                1)
        }, greater = {
            text.Output <- paste("True differences of relative effects are greater than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -Inf, upper = Tstat[pp], 
                  delta = rep(0, nc), corr = rhobf, df = dfT)
            }
            crit <- qmvnorm(conflevel, corr = rhobf, tail = "lower")$quantile
            Lower1 <- Cfisher - crit/sqrt(N) * sqrt(c(diag(Vfisher)))
            Lower <- (exp(2 * Lower1) - 1)/(exp(2 * Lower1) + 
                1)
            Upper <- rep(1, nc)
        })
    }, log.odds = { #updated for the log odds effect size
       	AsyMethod <- paste("Adjusted log odds with", round(dfT, rounds), "DF")
        Clogodds <- const * (log(Cpd.plus/(1-Cpd.plus)) - log(Cpd.minus/(1-Cpd.minus)))
        glogodds.plus <- Cpd.plus - Cpd.plus^2
        glogodds.minus <- Cpd.minus - Cpd.minus^2
        glogodds.plus.mat <- matrix(1,nrow=1,ncol=a) %x% matrix(glogodds.plus, ncol=1)
        glogodds.minus.mat <- matrix(1,nrow=1,ncol=a) %x% matrix(glogodds.minus, ncol=1)     
        Glogodds <- const * (Con.plus/glogodds.plus.mat - Con.minus/glogodds.minus.mat)
        	
        hVlogodds <- Glogodds %*% hVV
        Vlogodds <- hVlogodds %*% t(hVlogodds)
        Tstat <- sqrt(N) * Clogodds/sqrt(c(diag(Vlogodds)))
        switch(alternative, two.sided = {
            text.Output <- paste("True differences of relative effects are not equal to 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -abs(Tstat[pp]), upper = abs(Tstat[pp]), 
                  delta = rep(0, nc), corr = rhobf, df = dfT)[1]
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "both", 
                df = dfT)$quantile
            Lower <- Clogodds - crit/sqrt(N) * sqrt(c(diag(Vlogodds)))
            Upper <- Clogodds + crit/sqrt(N) * sqrt(c(diag(Vlogodds)))
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
            LowerPlot <- Clogodds - crit/sqrt(N) * sqrt(c(diag(Vlogodds)))
            Upper <- Clogodds + crit/sqrt(N) * sqrt(c(diag(Vlogodds)))
            UpperPlot <- Upper
        }, greater = {
            text.Output <- paste("True differences of relative effects are greater than 0")
            for (pp in 1:nc) {
                p.adj[pp] <- 1 - pmvt(lower = -Inf, upper = Tstat[pp], 
                  delta = rep(0, nc), corr = rhobf, df = dfT)
            }
            crit <- qmvt(conflevel, corr = rhobf, tail = "lower")$quantile
            Lower <- Clogodds - crit/sqrt(N) * sqrt(c(diag(Vlogodds)))
            LowerPlot <- Lower
            Upper <- rep(Inf, nc)
            UpperPlot <- Clogodds + crit/sqrt(N) * sqrt(c(diag(Vlogodds)))
        })
    })
    
    if (plot.simci == TRUE) {
        text.Ci <- paste(conflevel * 100, "%", "Simultaneous Confidence Intervals")
        Lowerp <- "|"
        
        #updated
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
            "\n", "-", "Estimation Method: ", text.output.W, 
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
    result$text.output.W <- text.output.W
    result$connames <- connames
    result$AsyMethod <- AsyMethod
    class(result) <- "mctp"
    return(result)
}
