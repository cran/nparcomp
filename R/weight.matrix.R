weight.matrix <-
function(n,type = c("UserDefined","Tukey","AVE","Dunnett", "Sequen",
                         "Changepoint", "Marcus",
                          "McDermott", "Williams", "UmbrellaWilliams"), base = 1, contrast.matrix=NULL) {
multcomp <- require(multcomp, quietly = TRUE)
a  <- length (n)

n.col <- a*(a-1)/2
tmp <- expand.grid(1:a, 1:a)
        ind <- tmp[[1]] > tmp[[2]]
        vi <- tmp[[2]][ind]
        vj <- tmp[[1]][ind]


type <- match.arg(type)

switch (type,
UserDefined = {
if (is.null(contrast.matrix)){stop("Choose a contrast or give a contrast matrix by using 'contrast.matrix = matrix'")}
 #if(any(abs(contrast.matrix)>1)){stop("The contrast weights must be between 0 and 1!")}
 if (ncol(contrast.matrix)!=a){stop("The contrast matrix has more or less columns than samples!")}
 nc<-nrow(contrast.matrix)

for (i in 1:nc){
places_pos<-(contrast.matrix[i,]>0)
places_neg<-(contrast.matrix[i,]<0)
sum_pos<-sum(contrast.matrix[i,][places_pos])
sum_neg<-sum(contrast.matrix[i,][places_neg])
if (abs(sum_neg)!= sum(sum_pos)) {
 stop(" Wrong contrast matrix!The sum of negative and positive weights must be equal") }

for ( j in 1:a){

if (contrast.matrix[i,j]<0){ contrast.matrix[i,j]<-contrast.matrix[i,j]/abs(sum_neg)}
if (contrast.matrix[i,j]>0){ contrast.matrix[i,j]<-contrast.matrix[i,j]/sum_pos}

}
 }
n.row <- nrow(contrast.matrix)


ch <- contrast.matrix
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w

},

AVE = {


n.row <- a

ch <- contrMat(n, type="AVE")
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
},

Changepoint = {

n.row <- a - 1

ch <- contrMat(n, type = "Changepoint")
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
},

Dunnett = {

n.row <- a - 1

ch <- contrMat(n, type = "Dunnett", base = base)
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
},

Sequen =  {

n.row <- a - 1

ch <- contrMat(n, type = "Sequen", base)
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
},

Marcus = {

n.row <- a*(a-1)/2

ch <- contrMat(n, type = "Marcus", base)
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
},

McDermott = {

n.row <- a -1

ch <- contrMat(n, type = "McDermott", base)
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
},

Williams = {

n.row <- a - 1

ch <- contrMat(n, type = "Williams")
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
} ,

Tukey = {


n.row <- a*(a-1)/2

ch <- contrMat(n, type = "Tukey", base)
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
},
UmbrellaWilliams = {

n.row <- a*(a-1)/2

ch <- contrMat(n, type = "UmbrellaWilliams", base)
w<-matrix(0,nrow = n.row , ncol=n.col )
weight.help2<-matrix(0,nrow = n.row , ncol=n.col )
weight.help1<-matrix(0,nrow = n.row , ncol=n.col )
for (i in 1:n.row){
for (j in 1:n.col){

help <- c(rep(i,n.col))
a <- help[j]
b <- vi[j]
d <- vj[j]

w[i,j] <- ch[a,b] *ch[a,d]

if (w[i,j]>0){w[i,j]<-0}
if (ch[a,b] > 0 && ch[a,d] < 0) {weight.help2[i,j]<-1}
if(ch[a,b] < 0 && ch[a,d] > 0){weight.help1[i,j] <-1}
}
}
w1<-w*weight.help1
w2<-w*weight.help2
w<--1*cbind(w1,w2)
w
}
)
ch.out <- matrix(c(ch),nrow=n.row)

result <- list(weight.matrix = w, weight.help1 = weight.help1,weight.help2 = weight.help2,contrast.matrix = ch.out)
result
}

