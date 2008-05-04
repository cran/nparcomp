"npar.t.test" <-
function(formula,
data,
conflevel=0.05,
alternative=c("two.sided","lower","greater"),
rounds=3,
asy.method=c("logit","probit", "normal", "t.app"),
p.permu = TRUE ,
plot.simci = TRUE,
info = TRUE
 )
    {


ssq <- function(x) {sum(x * x) }

logit <- function(p) { log(p/(1-p))  }

probit <- function(p) { qnorm(p)}

expit <- function(G){exp(G)/(1+exp(G))}


if (conflevel >= 1 || conflevel <= 0) {
      stop ("The confidence level must be between 0 and 1!")
if (is.null(alternative)){stop("Please declare the alternative! (two.sided, lower, greater)")}                                      }

alternative <- match.arg(alternative)
asy.method <- match.arg(asy.method)



    if (length(formula)!=3){stop("You can only analyse one-way layouts!")}
    dat <- model.frame(formula, data)
    if (ncol(dat) != 2) {
        stop("Specify one response and only one class variable in the formula")
    }
    if (is.numeric(dat[, 1]) == FALSE) {
        stop("Response variable must be numeric") }
    response <- dat[, 1]
    factorx <- as.factor(dat[, 2])

    fl <- levels(factorx)
    a <- nlevels(factorx)
    if (a >2){ stop("You want to perform a contrast test. Please use the function nparcomp")}
    samples <- split(response, factorx)
    n <- sapply(samples, length)
   if (any(n==1)) {warn<- paste("The factor level", fl[n==1], "has got only one observation!")
                    stop(warn)}
    ntotal <- sum(n)
    a <- length(n)
    nc <- 1
    dat<-data.frame(response,factorx)
     dat$factorx <- factor(dat$factorx)
    datord <- order(dat$factorx)
    dat <- data.frame(lapply(dat, "[", datord))

    cmpid <-paste("p(",fl[1],",",fl[2],")",sep="")

BF <- function(X,n1,n2){

N<-n1+n2
daten <- X

rdaten <- rank(daten)
rdaten1 <- rdaten[1:n1]
rdaten2 <- rdaten[(n1+1):N]
pd<-1/n1*(mean(rdaten2)-(n2+1)/2)
pd1 <- (pd == 1)
pd0 <- (pd == 0)
pd[pd1] <- 0.999
pd[pd0] <- 0.001
rges<-rank(c(daten[1:n1]))
rk<-rank(c(daten[(n1+1):N]))

z1<-1/n2*(rdaten[1:n1]-rges)
z2<-1/n1*(rdaten[(n1+1):N]-rk)

sqij<-var(z1)
sqji<-var(z2)
vd.bf<-N*sqij/n1+N*sqji/n2
singular.bf <- (vd.bf == 0)
vd.bf[singular.bf] <- N / (2*n1*n2)
t.bf<-sqrt(N)*(pd-1/2) / sqrt(vd.bf)
df.sw <- ( sqij/n1 +  sqji/n2)^2/(( sqij/n1)^2/(n1 -
      1) + ( sqji/n2)^2/(n2 - 1))
     df.sw[is.nan(df.sw)] <- 1000

     erg<- c(pd,vd.bf,t.bf,df.sw)
     erg
}

test <- BF(dat[,1],n[1],n[2])
pd <- test[1]
      vd.bf <- test[2]
      t.bf <- test[3]
      df.sw <- test[4]
if (p.permu == TRUE){

p.perm <- function(nperm,X,n0,n1,origin){
p.value<-c(rep(0,nperm))
for (i in 1:nperm){
Xpi <- sample(X)
t.bfstar <- BF(Xpi,n0,n1)[3]
p.value[i] <- ( origin <= t.bfstar )
}
mean(p.value)

}
p.permutation <- p.perm(10000,response,n[1],n[2],t.bf)
}
else {p.permutation <- NA}


      
      logit.pd <- logit(pd)
      logit.dev <- 1/(pd*(1-pd))
      vd.logit<-logit.dev^2*vd.bf
      t.logit <- (logit.pd) * sqrt(ntotal/vd.logit)



      probit.pd<-qnorm(pd)
      probit.dev<-sqrt(2*pi)/(exp(-0.5*qnorm(pd)*qnorm(pd)))
      vd.probit<-probit.dev^2*vd.bf
      t.probit <- (probit.pd) * sqrt(ntotal/vd.probit)


      if (alternative=="two.sided"){
      z.bft <- qt(1-conflevel/2,df.sw)
      z.bfn <- qnorm(1-conflevel/2)

      lower.bft <- pd-sqrt(vd.bf / ntotal)*z.bft
      upper.bft <- pd+sqrt(vd.bf / ntotal)*z.bft
      lower.bfn <- pd-sqrt(vd.bf / ntotal)*z.bfn
      upper.bfn <- pd+sqrt(vd.bf / ntotal)*z.bfn
      lower.logit <- expit(logit.pd-sqrt(vd.logit/ntotal)*z.bfn)
      upper.logit <- expit(logit.pd+sqrt(vd.logit/ntotal)*z.bfn)
      lower.probit <- pnorm(probit.pd-sqrt(vd.probit/ntotal)*z.bfn)
      upper.probit <- pnorm(probit.pd+sqrt(vd.probit/ntotal)*z.bfn)


      p.bfti<-pt(t.bf,df.sw)
      p.bfni<- pnorm(t.bf)
      p.bflogiti<- pnorm(t.logit)
      p.bfprobiti <- pnorm(t.probit)
      p.bft <- min (2-2*p.bfti,2*p.bfti)
      p.bfn <- min (2-2*p.bfni,2*p.bfni)
      p.bflogit<- min (2-2*p.bflogiti,2*p.bflogiti)
      p.bfprobit<- min (2-2*p.bfprobiti,2*p.bfprobiti)
      p.permutation <- min(2-2*p.permutation,2*p.permutation)




      text.output.p<-"H_0: p(i,j)=1/2"
      text.output.KI<-paste(100*(1-conflevel),"%","2-sided","Confidence-Interval for Relative Effect")
      upper <- "]"
      lower <- "["      }




      if (alternative=="lower"){
      z.bft <- qt(1-conflevel,df.sw)
      z.bfn <- qnorm(1-conflevel)

      lower.bft <- pd-sqrt(vd.bf/ntotal)*z.bft
      lower.bfn <- pd-sqrt(vd.bf/ntotal)*z.bfn
      lower.logit<-expit(logit.pd-sqrt(vd.logit/ntotal)*z.bfn)
      lower.probit<-pnorm(probit.pd-sqrt(vd.probit/ntotal)*z.bfn)
      upper.bft = upper.probit= upper.logit = upper.bfn = 1

      p.bft<- 1-pt(t.bf,df.sw)
      p.bfn<- 1- pnorm(t.bf)
      p.bflogit<- 1-pnorm(t.logit)
      p.bfprobit <- 1- pnorm(t.probit)
      p.permutation <- p.permutation


      text.output.p<-"H_0: p(i,j)<=1/2"
      text.output.KI<-paste(100*(1-conflevel),"%","1-sided","Confidence-Interval for Relative Effect")
      upper <- "]"
      lower <- "("
                              }


      if (alternative=="greater") {
      z.bft <- qt(1-conflevel,df.sw)
      z.bfn <- qnorm(1-conflevel)

      upper.bft<-pd+sqrt(vd.bf/ntotal)*z.bft
      upper.bfn<-pd+sqrt(vd.bf/ntotal)*z.bfn
      upper.logit<-expit(logit.pd+sqrt(vd.logit/ntotal)*z.bfn)
      upper.probit<-pnorm(probit.pd+sqrt(vd.probit/ntotal)*z.bfn)
      lower.bft = lower.probit= lower.logit = lower.bfn = 0

      p.bft <- pt(t.bf,df.sw)
      p.bfn<-  pnorm(t.bf)
      p.bflogit<- pnorm(t.logit)
      p.bfprobit <- pnorm(t.probit)
      p.permutation <- 1- p.permutation
      text.output.p <- " H_0: p(i,j)>=1/2"
      text.output.KI <- paste(100*conflevel,"%","1-sided","Confidence-Intervals for Relative Effect")
      upper <- ")"
      lower <- "["}



      bfn.output <- paste(lower ,round(lower.bfn,rounds),";",round(upper.bfn,rounds),upper)
      bft.output <- paste(lower ,round(lower.bft,rounds),";",round(upper.bft,rounds), upper)
      logit.output <- paste(lower,round(lower.logit,rounds),";",round(upper.logit,rounds),upper)
      probit.output <- paste(lower ,round(lower.probit,rounds),";",round(upper.probit,rounds),upper)
      p.bflogit <- round(p.bflogit,rounds)
      p.bfprobit <- round(p.bfprobit,rounds)
      p.bft <- round(p.bft,rounds)
      p.permutation <- round(p.permutation,rounds)
      p.bfn <- round(p.bfn,rounds)
      pd <- round(pd,rounds)
      vd.bf <- round (vd.bf,rounds)


wilcoxon <- data.frame(row.names=1,Comparison=cmpid,
      rel.effect = pd, p.value = wilcox.test(response~factorx,data,exact=FALSE)$p.value)
                      



data.info <- data.frame(row.names=1:2,Sample = fl, Size = n)


switch(   asy.method,

logit = {
      x.werte = cbind (lower.logit, pd, upper.logit)
      result <- list (
      Data.Info = data.info,
      Analysis.of.relative.effects =
      data.frame(row.names=1,Comparison=cmpid,
      rel.effect = pd ,
      confidence.interval = logit.output,
      t.value = t.logit,
      p.value = p.bflogit, p.perm = p.permutation),
      Wilcoxon.Test=wilcoxon)
      Asymptotic.Method <- " Delta-Method (Logit)"},

probit = {
      x.werte = cbind (lower.probit, pd, upper.probit)
      result <- list( Data.Info = data.info,
      Analysis.of.relative.effects =
      data.frame(row.names=c(1:nc),Comparison=cmpid,
      rel.effect = pd,
      confidence.interval = probit.output,
      t.value =t.probit,
      p.value =p.bfprobit,p.perm = p.permutation),
      Wilcoxon.Test=wilcoxon)
      Asymptotic.Method <- "Delta-Method (Probit)"} ,

normal = {
      x.werte = cbind (lower.bfn, pd, upper.bfn)
      result <- list( Data.Info = data.info,
      Analysis.of.relative.effects =
      data.frame ( row.names = c(1:nc),Comparison=cmpid,
      rel.effect = pd,
      confidence.interval=bfn.output,
      t.value = t.bf,
      p.value = p.bfn, p.perm = p.permutation),
      Wilcoxon.Test=wilcoxon)
      Asymptotic.Method <- "Normal Distribution"},

t.app = {
      x.werte = cbind (lower.bft, pd, upper.bft)
      result <- list(   Data.Info = data.info,
      Analysis.of.relative.effects =
      data.frame ( row.names = c(1:nc), Comparison=cmpid,
      rel.effect = pd,
      confidence.interval = bft.output,
      t.value = t.bf,
      p.value =p.bft, p.perm = p.permutation),
      Wilcoxon.Test=wilcoxon
     )

      Asymptotic.Method <- paste(" t - Distribution with d.f.= ",round(df.sw,4))}
)



if ( plot.simci == TRUE ) {

      test <- matrix(c(1:nc),ncol=nc,nrow=nc)
      angaben<-c(cmpid)
      angaben<-matrix(c(angaben),ncol=nc,nrow=nc)
      k<-c(1:nc)
      plot(x.werte[,2],k,xlim=c(0,1),axes = FALSE, type = "p",pch=15,xlab = "",ylab = "")
      abline(v=0.5,col="red",lty = 1, lwd = 2)
      axis(1, at =seq(0,1,0.1))
      axis(2, at = test, labels = angaben)
      axis(4,at = test, labels = test)


      points (x = x.werte[,3], y = test[,1],pch = upper )
      points(x = x.werte[,1], y = test[,1], pch = lower )
      for (i in 1:nc){
      polygon(c(x.werte[i,1],x.werte[i,3]),c(i,i))
      }
      box()

      title( main = c(text.output.KI,paste("Method:","", Asymptotic.Method,sep="")),
      ylab ="Comparison",
      xlab = paste("lower",lower,"-----","p","------",upper,"upper"))
                              }

  if (info == TRUE){
      cat("\n","", "Nonparametric Behrens-Fisher Problem","\n",
      "NOTE:","\n",
      "*-----------Analysis of relative effects-----------*","\n",
      "-", "Confidence interval for relative effect p(i,j)
      with confidence level",1-conflevel,"\n",
      "-","Method","=",Asymptotic.Method,"\n",
      "-","p.perm","=","p-value of the Neubert-Brunner permutation test","\n",
      "-","p-Values for ",text.output.p,"\n","\n",
      "*----------------Interpretation--------------------*","\n",
      "p(a,b)", ">", "1/2", ":", "b tends to be larger than a","\n",
      "*-------------------Wilcox.Test--------------------*","\n",
      "-", "Asymptotic Wilcoxon Test","\n",
      "-", "In this setup you can only test H_0:F_i = F_j","\n",
      "*--------------------------------------------------*","\n")}




return(result)


}

