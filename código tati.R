
library(sn)
n = 100
beta0 = -1
beta1 = -4
beta2 = -3
nu = 3
sigma2 = 2
lambda = 2

y <- x1 <- x2 <- vector()

for(i in 1:n){
  x[i] = runif(1,-2,2)
  E[i] = rsn(1,-sqrt(2/pi)*nu,sigma2,lambda)
  y[i] = beta0 + beta1*x[i] + E[i]
 
}

d<-sample(1:100, 3, replace=FALSE, prob=NULL)
y1<-(1+(z/100))*y[d[1]]
y2<-(1+(z/100))*y[d[2]]
y3<-(1+(z/100))*y[d[3]]

w<-y[-c(d[1],d[2],d[3])]
w<-c(w,y1,y2,y2)