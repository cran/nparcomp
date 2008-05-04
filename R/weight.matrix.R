"weight.matrix" <-
function(n,type = c("Tukey","AVE","Dunnett", "Sequen",
                         "Changepoint", "Marcus", 
                          "McDermott", "Williams"), base = 1) {
multcomp <- require(multcomp, quietly = TRUE)
a  <- length (n)

n.col <- a*(a-1)/2
tmp <- expand.grid(1:a, 1:a)
        ind <- tmp[[1]] > tmp[[2]]
        vi <- tmp[[2]][ind]
        vj <- tmp[[1]][ind]

type <- match.arg(type)
switch (type, AVE = {


n.row <- a

ch <- contrMat(n, type="AVE")
w<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}

}
}
w<--1*w
w
},

Changepoint = { 

n.row <- a - 1

ch <- contrMat(n, type = "Changepoint")
w<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
hilf2 <- vi
hilf4 <- vj
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}

}
}
w<--1*w
w
},

Dunnett = {   

n.row <- a - 1

ch <- contrMat(n, type = "Dunnett", base=base)
w<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
hilf2 <- vi
hilf4 <- vj
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}

}
}
w<--1*w
w
},

Sequen =  {

n.row <- a - 1

ch <- contrMat(n, type = "Sequen", base)
w<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
hilf2 <- vi
hilf4 <- vj
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}

}
}
w<--1*w
w
},

Marcus = {

n.row <- a*(a-1)/2

ch <- contrMat(n, type = "Marcus", base)
w<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
hilf2 <- vi
hilf4 <- vj
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}

}
}
w<--1*w
w
},

McDermott = { 

n.row <- a -1 

ch <- contrMat(n, type = "McDermott", base)
w <- matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))

a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}

}
}
w<--1*w
w
},

Williams = {

n.row <- a - 1

ch <- contrMat(n, type = "Williams")
w<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
hilf2 <- vi
hilf4 <- vj
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}

}
}
w<--1*w
w
} ,

Tukey = {


n.row <- a*(a-1)/2

ch <- contrMat(n, type = "Tukey", base)
w <- matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
hilf2 <- vi
hilf4 <- vj
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}

}
}
w<--1*w

}




)
rownames(w)<-paste("C",1:n.row)
colnames(w) <- 1:n.col
w
}

