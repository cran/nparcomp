##' Nonparametric relative contrast effects
##' 
##' The function nparcomp computes the estimator of nonparametric relative
##' contrast effects, simultaneous confidence intervals for the effects and
##' simultaneous p-values based on special contrasts like "Tukey", "Dunnett",
##' "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus",
##' "UmbrellaWilliams", "UserDefined". The statistics are computed using
##' multivariate normal distribution, multivariate Satterthwaite
##' t-Approximation and multivariate transformations (Probit and Logit
##' transformation function). The function 'nparcomp' also computes one-sided
##' and two-sided confidence intervals and p-values. The confidence intervals
##' can be plotted.
##' 
##' 
##' @param formula A two-sided 'formula' specifying a numeric response variable
##' and a factor with more than two levels. If the factor contains less than 3
##' levels, an error message will be returned.
##' @param data A dataframe containing the variables specified in formula.
##' @param type Character string defining the type of contrast. It should be
##' one of "Tukey", "Dunnett", "Sequen", "Williams", "Changepoint", "AVE",
##' "McDermott", "Marcus", "UmbrellaWilliams", "UserDefined".
##' @param control Character string defining the control group in Dunnett
##' comparisons. By default it is the first group by definition of the dataset.
##' @param conf.level The confidence level for the conflevel confidence
##' intervals (default is 0.95).
##' @param alternative Character string defining the alternative hypothesis,
##' one of "two.sided", "less" or "greater".
##' @param rounds Number of rounds for the numeric values of the output. By
##' default it is rounds=3.
##' @param correlation A logical whether the estimated correlation matrix and
##' covariance matrix should be printed.
##' @param asy.method Character string defining the asymptotic approximation
##' method, one of "logit", for using the logit transformation function,
##' "probit", for using the probit transformation function, "normal", for using
##' the multivariate normal distribution or "mult.t" for using a multivariate
##' t-distribution with a Satterthwaite Approximation.
##' @param plot.simci A logical indicating whether you want a plot of the
##' confidence intervals.
##' @param info A logical whether you want a brief overview with informations
##' about the output.
##' @param contrast.matrix User defined contrast matrix.
##' @param weight.matrix A logical indicating whether the weight matrix should
##' be printed.
##' @return
##' 
##' \item{Data.Info }{List of samples and sample sizes. }
##' \item{Contrast}{Contrast matrix.} \item{Analysis }{Comparison: relative
##' contrast effect , relative.effect: estimated relative contrast effect,
##' Estimator: Estimated relative contrast effect, Lower: Lower limit of the
##' simultaneous confidence interval, Upper: Upper limit of the simultaneous
##' confidence interval, Statistic: Teststatistic p.Value: Adjusted p-values
##' for the hypothesis by the choosen approximation method.  } \item{ input
##' }{List of input by user.}
##' @note If the samples are completely seperated the variance estimators are
##' Zero by construction. In these cases the Null-estimators are replaced by
##' 0.001. Estimated relative effects with 0 or 1 are replaced with 0.001,
##' 0.999 respectively.
##' 
##' A summary and a graph can be created separately by using the functions
##' \code{\link{summary.nparcomp}} and \code{\link{plot.nparcomp}}.
##' 
##' For the analysis, the R packages 'multcomp' and 'mvtnorm' are required.
##' @author Frank Konietschke
##' @seealso For two-sample comparisons based on relative effects, see
##' \code{\link{npar.t.test}}.
##' @references Konietschke, F., Brunner, E., Hothorn, L.A. (2008).
##' Nonparametric Relative Contrast Effects: Asymptotic Theory and Small Sample
##' Approximations.
##' 
##' Munzel. U., Hothorn, L.A. (2001). A unified Approach to Simultaneous Rank
##' Tests Procedures in the Unbalanced One-way Layout. Biometric Journal, 43,
##' 553-569.
##' @keywords htest
##' @examples
##' 
##' 
##' data(liver)
##' 
##'   # Williams Contrast
##' 
##' a<-nparcomp(weight ~dosage, data=liver, asy.method = "probit",
##'             type = "Williams", alternative = "two.sided", 
##'             plot.simci = TRUE, info = FALSE,correlation=TRUE)
##' summary(a)
##' 
##' 
##' 
##'  # Dunnett dose 3 is baseline
##' 
##' c<-nparcomp(weight ~dosage, data=liver, asy.method = "probit",
##'             type = "Dunnett", control = "3",
##'             alternative = "two.sided", info = FALSE)
##' summary(c)
##' plot(c)
##' 
##' 
##' data(colu)
##' 
##'   # Tukey comparison- one sided(lower)
##' 
##' a<-nparcomp(corpora~ dose, data=colu, asy.method = "mult.t",
##'             type = "Tukey",alternative = "less", 
##'             plot.simci = TRUE, info = FALSE)
##' summary(a)
##' 
##' 
##'  # Tukey comparison- one sided(greater)
##' 
##' b<-nparcomp(corpora~ dose, data=colu, asy.method = "mult.t",
##'             type = "Tukey",alternative = "greater", 
##'             plot.simci = TRUE, info = FALSE)
##' summary(b)
##' 
##' 
##' 
##' @export nparcomp
nparcomp <-
function (formula, data, type = c("Tukey", "Dunnett",
    "Sequen", "Williams", "Changepoint", "AVE", "McDermott",
    "Marcus", "UmbrellaWilliams", "UserDefined"), control = NULL, conf.level = 0.95,
    alternative = c("two.sided", "less", "greater"), rounds = 3,
    correlation = FALSE, asy.method = c("logit", "probit", "normal",
        "mult.t"), plot.simci = FALSE, info = TRUE,contrast.matrix=NULL, weight.matrix=FALSE)
{

input.list <- list(formula = formula, data = data, type = type[1],
                   conf.level=conf.level, alternative=alternative,
                   asy.method=asy.method, plot.simci=plot.simci,
                   control=control, info=info, rounds=rounds, 
                   contrast.matrix=contrast.matrix, correlation=correlation,
                   weight.matrix=weight.matrix)

#----------------------Necessary Functions-------------------------------------#
ssq <- function(x) {sum(x * x)}
logit <- function(p){log(p/(1 - p))}
probit <- function(p){qnorm(p)}
expit <- function(G){exp(G)/(1 + exp(G))}

conflevel<-conf.level
#---------------------Containment of given Parameters--------------------------#
if (conflevel >= 1 || conflevel <= 0) {
stop("The confidence level must be between 0 and 1!")
if (is.null(alternative)) {
            stop("Please declare the alternative! (two.sided, lower, greater)")
        }
    }
    
type <- match.arg(type)
alternative <- match.arg(alternative)
asy.method <- match.arg(asy.method)

#---------------------------Arrange the data-----------------------------------#
if (length(formula) != 3) {stop("You can only analyse one-way layouts!")}
    dat <- model.frame(formula, droplevels(data))
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
        stop("You want to perform a two-sample test. Please use the function npar.t.test")
    }
    samples <- split(response, factorx)
    n <- sapply(samples, length)
    if (any(n <= 1)) {
        warn <- paste("The factor level", fl[n <= 1], "has got only one observation!")
        stop(warn)
    }
    N <- sum(n)
    a <- length(n)
    
#----------------------------Rank the data-------------------------------------#
    tmp <- expand.grid(1:a, 1:a)
    ind <- tmp[[1]] > tmp[[2]]
    vi <- tmp[[2]][ind]
    vj <- tmp[[1]][ind]
    nc <- length(vi)
    gn <- n[vi] + n[vj]
    intRanks <- lapply(samples, rank)
    pairRanks <- lapply(1:nc, function(arg) {
        rank(c(samples[[vi[arg]]], samples[[vj[arg]]]))
    })
    pd1 <- sapply(1:nc, function(arg) {
        i <- vi[arg]
        j <- vj[arg]
        (sum(pairRanks[[arg]][(n[i] + 1):gn[arg]])/n[j] - (n[j] +
            1)/2)/n[i]
    })
    dij <- dji <- list(0)
    sqij <- sapply(1:nc, function(arg) {
        i <- vi[arg]
        j <- vj[arg]
        pr <- pairRanks[[arg]][(n[i] + 1):gn[arg]]
        dij[[arg]] <<- pr - sum(pr)/n[j] - intRanks[[j]] + (n[j] +
            1)/2
        ssq(dij[[arg]])/(n[i] * n[i] * (n[j] - 1))
    })
    sqji <- sapply(1:nc, function(arg) {
        i <- vi[arg]
        j <- vj[arg]
        pr <- pairRanks[[arg]][1:n[i]]
        dji[[arg]] <<- pr - sum(pr)/n[i] - intRanks[[i]] + (n[i] + 1)/2
        ssq(dji[[arg]])/(n[j] * n[j] * (n[i] - 1))
    })
    vd.bf <- N * (sqij/n[vj] + sqji/n[vi])
    singular.bf <- (vd.bf == 0)
    vd.bf[singular.bf] <- 1e-05


    cov.bf1 <- diag(nc)
    rho.bf <- diag(nc)
    for (x in 1:(nc - 1)) {
        for (y in (x + 1):nc) {
            i <- vi[x]
            j <- vj[x]
            v <- vi[y]
            w <- vj[y]
            p <- c(i == v, j == w, i == w, j == v)
            if (sum(p) == 1) {
                cl <- list(function() (t(dji[[x]]) %*% dji[[y]])/(n[j] *
                  n[w] * n[i] * (n[i] - 1)), function() (t(dij[[x]]) %*%
                  dij[[y]])/(n[i] * n[v] * n[j] * (n[j] - 1)),
                  function() -(t(dji[[x]]) %*% dij[[y]])/(n[v] *
                    n[j] * n[i] * (n[i] - 1)), function() -(t(dij[[x]]) %*%
                    dji[[y]])/(n[i] * n[w] * n[j] * (n[j] - 1)))
                case <- (1:4)[p]
                rho.bf[x, y] <- rho.bf[y, x] <- sqrt(N*
                  N)/sqrt(vd.bf[x] * vd.bf[y]) * cl[[case]]()
                cov.bf1[x, y] <- cov.bf1[y, x] <- sqrt(vd.bf[x] *
                  vd.bf[y])
            }
        }
    }
    V <- (cov.bf1 + diag(vd.bf - 1)) * rho.bf
    cov.bf1 <- cbind(V, -1 * V)
    cov.bf2 <- cbind(-1 * V, V)
    cov.bf <- rbind(cov.bf1, cov.bf2)

#----------------------Organize the Contrast Matrix----------------------------#

#--------------------Special Arguments for User Defined Contrasts--------------#
if (type=="UserDefined"){
if(is.null(contrast.matrix)){stop("Please enter a contrast matrix!")}
Con<-contrast.matrix
rownames(Con)<-paste("C",1:nrow(Con))
for (rc in 1:nrow(Con)){if(sum(Con[rc,][Con[rc,]>0])!=1){stop("Sums of positive contrast coefficients must be 1!")}}
colnames(Con)<-fl}

#---------------------------Pre-Specified Contrasts----------------------------#
if(type!="UserDefined"){

#---------------------------Organize the Control Group-------------------------#
if (is.null(control)){icon<-1}
if (!is.null(control)){icon<-which(fl==control)}

#----------------------------Get the Contrast Matrix---------------------------#
Con<-contrMat(n=n,type,icon)}

nc<-nrow(Con)
a1<-a*(a-1)
cmpid<-rownames(Con)
ch<-matrix(Con,ncol=a)
colnames(ch)<- colnames(Con)
rownames(ch) <- cmpid

if (type=="Dunnett" || type=="Tukey" || type == "Sequen") {
for (t1 in 1:nc){
cmpid[t1]<-paste("p(",fl[which(Con[t1,]==-1)],",", fl[which(Con[t1,]==1)],")")
}
}

#---------------------------Compute the weight matrix--------------------------#
vii<-c(vi,vj)
vjj<-c(vj,vi)
W <- matrix(0, nrow = nrow(ch), ncol = a*(a-1))

for (ss in 1:nrow(ch)){
for (ll in 1:a1){
h1<-vii[ll]
h2<-vjj[ll]

if(ch[ss,h1]<0 && ch[ss,h2]>0){
W[ss,ll]<-abs(ch[ss,h1]*ch[ss,h2])}
}}




#--------------------------Compute the Point Estimators------------------------#

pd.help1 <- c(pd1, 1 - pd1)
pd <- c(W %*% pd.help1)

pd11 <- (pd == 1)
pd00 <- (pd == 0)
pd[pd11] <- 0.999
pd[pd00] <- 0.001

cov.bf <- W %*% cov.bf %*% t(W)

variances.bf <- c(diag(cov.bf))

#-------------------Compute the Test Statistics--------------------------------#
rho.bf <- cov2cor(cov.bf)

p.adj<-c()
switch(asy.method,
#--------------------------Logit-Transformation--------------------------------#
logit={
logit.pd <- logit(pd)
    logit.dev <- diag(1/(pd * (1 - pd)))
    logit.cov <- logit.dev %*% cov.bf %*% t(logit.dev)
    vd.logit <- c(diag(logit.cov))
    T <- (logit.pd) * sqrt(N/vd.logit)
AsyMethod <- "Logit - Transformation"

switch(alternative,
#---------------------Two-Sided Alternative------------------------------------#
two.sided = {
text.Output <- paste("True relative contrast effect p is not equal to 1/2")
for (pp in 1:nc) {
p.adj[pp] <- 1 - pmvnorm(lower = -abs(T[pp]),abs(T[pp]), corr = rho.bf, mean = rep(0,nc))
}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "both")$quantile
Lower1 <- logit.pd - crit/sqrt(N)*sqrt(c(diag(logit.cov)))
Upper1 <- logit.pd + crit/sqrt(N)*sqrt(c(diag(logit.cov)))
Lower <- expit(Lower1)
Upper <- expit(Upper1)
},

less={
text.Output <- paste("True relative contrast effect p is less than 1/2")
 for (pp in 1:nc) {
p.adj[pp] <- pmvnorm(lower = -Inf, T[pp], corr = rho.bf, mean = rep(0,nc))
}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "lower")$quantile
Upper1 <- logit.pd + crit/sqrt(N)*sqrt(c(diag(logit.cov)))
Lower <- rep(0,nc)
Upper <- expit(Upper1)
},

greater={
text.Output <- paste("True relative contrast effect p is greater than 1/2")
for (pp in 1:nc) {
p.adj[pp]<-1-pmvnorm(lower =-Inf , upper =T[pp],
                mean = rep(0, nc), corr = rho.bf)}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "lower")$quantile
Lower1 <- logit.pd - crit/sqrt(N)*sqrt(c(diag(logit.cov)))
Lower <- expit(Lower1)
Upper <- rep(1,nc)
}
)
},

#----------------------Probit-Transformation-----------------------------------#

probit ={
probit.pd <- qnorm(pd)
    probit.dev <- diag(sqrt(2 * pi)/(exp(-0.5 * qnorm(pd) * qnorm(pd))))
    probit.cov <- probit.dev %*% cov.bf %*% t(probit.dev)
    T <- (probit.pd) * sqrt(N/c(diag(probit.cov)))
AsyMethod <- "Probit - Transformation"
switch(alternative,
#---------------------Two-Sided Alternative------------------------------------#
two.sided = {
text.Output <- paste("True relative contrast effect p is not equal to 1/2")

for (pp in 1:nc) {
p.adj[pp] <- 1 - pmvnorm(lower = -abs(T[pp]),abs(T[pp]), corr = rho.bf, mean = rep(0,nc))
}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "both")$quantile
Lower1 <- probit.pd - crit/sqrt(N)*sqrt(c(diag(probit.cov)))
Upper1 <- probit.pd + crit/sqrt(N)*sqrt(c(diag(probit.cov)))
Lower <- pnorm(Lower1)
Upper <- pnorm(Upper1)
},

less={
text.Output <- paste("True relative contrast effect p is less than 1/2")
 for (pp in 1:nc) {
p.adj[pp] <- pmvnorm(lower = -Inf, T[pp], corr = rho.bf, mean = rep(0,nc))
}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "lower")$quantile
Upper1 <- probit.pd + crit/sqrt(N)*sqrt(c(diag(probit.cov)))
Lower <- rep(0,nc)
Upper <- pnorm(Upper1)
},

greater={
text.Output <- paste("True relative contrast effect p is greater than 1/2")
for (pp in 1:nc) {
p.adj[pp]<-1-pmvnorm(lower =-Inf , upper =T[pp],
                mean = rep(0, nc), corr = rho.bf)}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "lower")$quantile
Lower1 <- probit.pd - crit/sqrt(N)*sqrt(c(diag(probit.cov)))
Lower <- pnorm(Lower1)
Upper <- rep(1,nc)
}
)
},

#--------------------------Multi-Normal Approximation--------------------------#
normal ={
AsyMethod <- "Normal - Approximation"
    T <- sqrt(N) * (pd - 1/2)/sqrt(variances.bf)

switch(alternative,
#---------------------Two-Sided Alternative------------------------------------#
two.sided = {
text.Output <- paste("True relative contrast effect p is not equal to 1/2")
for (pp in 1:nc) {
p.adj[pp] <- 1 - pmvnorm(lower = -abs(T[pp]),abs(T[pp]), corr = rho.bf, mean = rep(0,nc))
}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "both")$quantile
Lower <- pd - crit/sqrt(N)*sqrt(variances.bf)
Upper <- pd + crit/sqrt(N)*sqrt(variances.bf)

},

less={
text.Output <- paste("True relative contrast effect p is less than 1/2")
 for (pp in 1:nc) {
p.adj[pp] <- pmvnorm(lower = -Inf, T[pp], corr = rho.bf, mean = rep(0,nc))
}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "lower")$quantile
Lower <- rep(0,nc)
Upper <- pd + crit/sqrt(N)*sqrt(variances.bf)
},

greater={
text.Output <- paste("True relative contrast effect p is greater than 1/2")
for (pp in 1:nc) {
p.adj[pp]<-1-pmvnorm(lower =-Inf , upper =T[pp],
                mean = rep(0, nc), corr = rho.bf)}
crit<- qmvnorm(conflevel, corr = rho.bf, tail = "lower")$quantile
Lower <- pd - crit/sqrt(N)*sqrt(variances.bf)
Upper <- rep(1,nc)
}
)
},

#-------------------------Multi-t-Approximation--------------------------------#
mult.t ={

df.sw <- (n[vi] * sqij + n[vj] * sqji)^2/((n[vi] * sqij)^2/(n[vj] -
        1) + (n[vj] * sqji)^2/(n[vi] - 1))
df.sw[is.nan(df.sw)] <- 1000
    df.sw <- W %*% c(df.sw, df.sw)
    df.sw <- max(4, min(df.sw))
df.sw<-round(df.sw)
T <- sqrt(N) * (pd - 1/2)/sqrt(variances.bf)
AsyMethod <- paste("Multi - T with", round(df.sw,rounds), "DF")
switch(alternative,
#---------------------Two-Sided Alternative------------------------------------#
two.sided = {
text.Output <- paste("True relative contrast effect p is not equal to 1/2")
for (pp in 1:nc) {
p.adj[pp] <- 1 - pmvt(lower = -abs(T[pp]),abs(T[pp]), corr = rho.bf,  df= df.sw, delta = rep(0,nc))
}
crit<- qmvt(conflevel, df=df.sw, corr = rho.bf, tail = "both")$quantile
Lower <- pd - crit/sqrt(N)*sqrt(variances.bf)
Upper <- pd + crit/sqrt(N)*sqrt(variances.bf)

},

less={
text.Output <- paste("True relative contrast effect p is less than 1/2")
 for (pp in 1:nc) {
p.adj[pp] <- pmvt(lower = -Inf, T[pp], corr = rho.bf, delta = rep(0,nc),df=df.sw)}
crit<- qmvt(conflevel, corr = rho.bf, tail = "lower",df=df.sw)$quantile
Lower <- rep(0,nc)
Upper <- pd + crit/sqrt(N)*sqrt(vd.bf)
},

greater={
text.Output <- paste("True relative contrast effect p is greater than 1/2")
for (pp in 1:nc) {
p.adj[pp]<-1-pmvt(lower =-Inf , upper =T[pp],
                delta = rep(0, nc), corr = rho.bf, df=df.sw)}
crit<- qmvt(conflevel, corr = rho.bf, tail = "lower", df=df.sw)$quantile
Lower <- pd - crit/sqrt(N)*sqrt(variances.bf)
Upper <- rep(1,nc)
}
)
}
)

#-------------------------Plot of the SCI--------------------------------------#

if (plot.simci == TRUE) {
text.Ci<-paste(conflevel*100, "%", "Simultaneous Confidence Intervals")
 Lowerp<-"|"
       plot(pd,1:nc,xlim=c(0,1), pch=15,axes=FALSE,xlab="",ylab="")
       points(Lower,1:nc, pch=Lowerp,font=2,cex=2)
              points(Upper,1:nc, pch=Lowerp,font=2,cex=2)
              abline(v=0.5, lty=3,lwd=2)
              for (ss in 1:nc){
              polygon(x=c(Lower[ss],Upper[ss]),y=c(ss,ss),lwd=2)}
              axis(1, at = seq(0, 1, 0.1))
              axis(2,at=1:nc,labels=cmpid)
                box()
 title(main=c(text.Ci, paste("Type of Contrast:",type), paste("Method:", AsyMethod) ))


 }



    data.info <- data.frame(row.names = 1:a, Sample = fl, Size = n)
    Analysis <- data.frame(Comparison=cmpid, Estimator=round(pd,rounds), Lower=round(Lower,rounds), Upper=round(Upper,rounds), Statistic=T, p.Value=p.adj)
    Overall<-data.frame(Quantile=crit, p.Value=min(p.adj))
    result<-list(Data.Info=data.info, Contrast=ch, Analysis=Analysis, Overall=Overall)

     if (info == TRUE) {
        cat("\n", "#------Nonparametric Multiple Comparisons for relative contrast effects-----#", "\n","\n",
        "-", "Alternative Hypothesis: ", text.Output,"\n",
        "-", "Type of Contrast", ":", type, "\n", "-", "Confidence level:",
            conflevel*100,"%", "\n", "-", "Method", "=", AsyMethod,"\n",
                    "-", "Estimation Method: Pairwise rankings","\n", "\n",
                    "#---------------------------Interpretation----------------------------------#",
            "\n", "p(a,b)", ">", "1/2", ":", "b tends to be larger than a","\n",
            "#---------------------------------------------------------------------------#", "\n",

            "\n")
    }
    
#------------------------Informations on the Weight-Matrix---------------------#
if (weight.matrix==TRUE){
colW<-c()
for (wc in 1:a1){colW[wc]<-paste("p(",fl[vii[wc]],",", fl[vjj[wc]],")")}
colnames(W)<-colW
result$Weight.Matrix<-W
result$AllPairs<-data.frame(Pairs=colW, Effect=pd.help1)}
#------------------------Informations on the Correlation Matrix-----------------#
if (correlation == TRUE){
result$Covariance<-cov.bf
result$Correlation <- rho.bf
}

#------------------------Give the Result---------------------------------------#
result$input<-input.list
result$text.Output<-text.Output
result$connames<-cmpid
result$AsyMethod<-AsyMethod
class(result)<-"nparcomp"
return(result)
}
